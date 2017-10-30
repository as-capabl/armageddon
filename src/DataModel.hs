{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

-- Global event
module
    DataModel
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Lens hiding (set)
import Control.Concurrent (forkIO)
import Control.Monad (forM, mzero, mplus)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe
import Control.Exception (bracket)
import qualified Control.Monad.Trans.Resource as R

import Control.Arrow.Machine
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner
import Control.Arrow.Machine.ConduitAdaptor
import Graphics.UI.McGtk (GtkRunner)

import qualified Data.Text as Text
import Data.Maybe (listToMaybe, catMaybes)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Attoparsec as CA

import Database.Relational.Query
import Database.HDBC (commit, disconnect)
import Database.HDBC.Sqlite3
import Database.HDBC.Query.TH
import Database.HDBC.Record
import System.FilePath
import System.Directory

import BasicModel
import qualified AuthDB.Init as AuthDB
import qualified AuthDB.Types as AuthDB

import qualified Web.Hastodon as Hdon

type TheWorld = World IO IO GtkRunner
type TheMBox = Mailbox IO IO GtkRunner


data Unordered a = Add a | Del a | Clear
makePrisms ''Unordered

--
-- Data model type
--
data T = T {
    _regMBox :: TheMBox (Unordered Registration),
    _updateRPHMBox :: TheMBox (BMText, [Hdon.Status], Bool),
    _updateRPHNMBox :: TheMBox (BMText, [Hdon.Notification], Bool),
    _updateStMBox :: TheMBox Hdon.Status,
    _selDSMBox :: TheMBox DataSource
  }
makeLenses ''T

init ::
    (HasWorld IO IO GtkRunner i, Occasional o) =>
    Evolution i o IO T
init =
  do
    regB <- mailboxNew
    selDSB <- mailboxNew
    updateRPHB <- mailboxNew
    updateRPHNB <- mailboxNew
    updateSt <- mailboxNew
    return $ T {
        _regMBox = regB,
        _updateRPHMBox = updateRPHB,
        _updateRPHNMBox = updateRPHNB,
        _updateStMBox = updateSt,
        _selDSMBox = selDSB
      }

--
-- Internal
--
getAuthConn :: R.ResourceT IO Connection
getAuthConn =
  do
    (_, conn) <- R.allocate AuthDB.prepareAuth disconnect
    return conn

dbReg :: Prism' AuthDB.Registration Registration
dbReg = prism' regTo regFrom
  where
    regTo (Registration hst un tok) =
        AuthDB.Registration (Just hst) (Just un) (Just tok)
    regFrom reg@(AuthDB.Registration hst un tok) =
        Registration <$> hst <*> un <*> tok

dbHost :: Prism' AuthDB.Host Host
dbHost = prism' hostTo hostFrom
  where
    hostTo (Host hn cid cs) =
        AuthDB.Host (Just hn) (Just cid) (Just cs)
    hostFrom h@(AuthDB.Host hn cid cs) =
        Host <$> hn <*> cid <*> cs

writeReg :: Registration -> IO ()
writeReg reg = R.runResourceT $
  do
    conn <- getAuthConn
    lift $ runInsert conn (derivedInsert id') (reg ^. re dbReg)
    lift $ commit conn

readRegs :: T -> ProcessT (R.ResourceT IO) (Event ()) (Event Registration)
readRegs model = constructT $
  do
    conn <- lift $ getAuthConn
    regs <- liftIO $
        runQuery conn (relationalQuery AuthDB.registration) ()
    forM regs $ \x ->
      do
        mapM_ yield $! x ^? dbReg
    return ()

writeHost :: Host -> IO ()
writeHost hst = R.runResourceT $
  do
    conn <- getAuthConn
    liftIO $
      do
        runDelete conn `flip` () $ derivedDelete $ \h ->
          do
            wheres $ h ! AuthDB.hostname' .=. value (Just (hst ^. hostname))
            return unitPlaceHolder
        runInsert conn (derivedInsert id') (hst ^. re dbHost)
        commit conn
    return ()

findHost :: String -> IO (Maybe Host)
findHost hn = R.runResourceT $
  do
    conn <- getAuthConn
    hsts <- liftIO $ runQuery conn `flip` () $ relationalQuery . relation $
      do
        h <- query AuthDB.host
        wheres $ h ! AuthDB.hostname' .=. value (Just (Text.pack hn))
        return h
    return $ listToMaybe hsts >>= (^? dbHost)

--
-- Accessors
--
loadSetting :: T -> IO ()
loadSetting model =
  do
    forkIO $ R.runResourceT $ runT_ (readRegs model >>> fire postAuth) (repeat ())
    return ()
  where
    postAuth reg =
        liftIO $ mailboxPost (model ^. regMBox) $ Add reg

getClientInfo :: String -> String -> IO (Maybe Host)
getClientInfo hostname appname = runMaybeT $ MaybeT (findHost hostname)`mplus` newClient
  where
    newClient =
      do
        oc <- lift $ Hdon.postApps hostname appname
        lift $ print oc
        hst <- maybe mzero return $ oauthToHost oc
        lift $ writeHost hst
        return hst

    oauthToHost (Left _) = Nothing
    oauthToHost (Right Hdon.OAuthClient{..}) =
        let
            hn = Text.pack hostname
            cid = Text.pack oauthClientClientId
            cs = Text.pack oauthClientClientSecret
          in
            Just $ Host hn cid cs

addRegistration :: T -> Registration -> IO ()
addRegistration model reg =
  do
    writeReg reg
    mailboxPost (model ^. regMBox) $ Add reg

onAddReg ::
    T -> ProcessT IO TheWorld (Event Registration)
onAddReg model = proc world ->
  do
    ureg <- onMailboxPost $ model ^. regMBox -< world
    returnA -< filterJust $ (^? _Add) <$> ureg

selDS :: T -> DataSource -> IO ()
selDS model ds =
  do
    mailboxPost (model ^. selDSMBox) $ ds

selUserDSByStatusId :: T -> Registration -> Int -> IO ()
selUserDSByStatusId model reg statusId = fmap (const ()) $ forkIO $
  do
    Right st <- Hdon.getStatus (reg ^. hastodonClient) statusId
    let account = Hdon.statusAccount st
        accountId = Hdon.accountId account
    selDS model $ DataSource reg (DSS $ DSUserStatus accountId)

onSelDS :: T -> ProcessT IO TheWorld (Event DataSource)
onSelDS model = proc world ->
  do
    onMailboxPost $ model ^. selDSMBox -< world

readDSS :: DataSource' DSSKind -> ProcessT (R.ResourceT IO) (Event ()) (Event Hdon.Status)
readDSS ds0 = constructT $
  do
    auto $ (C.catchC (sourceReadDs ds0) (liftIO . printEx)) C.=$= filterUpdateC
  where
    sourceReadDs ds@(DataSource _ DSHome) = Hdon.sourceUserTimeline (ds ^. hastodonClient)
    sourceReadDs (DataSource _ (DSUserStatus _)) = return ()
    sourceReadDs ds = Hdon.sourcePublicTimeline (ds ^. hastodonClient)

    filterUpdateC = C.awaitForever $ \case
        Hdon.StreamUpdate x -> C.yield x
        _ -> return ()

    printEx :: CA.ParseError -> IO ()
    printEx = print

readDSN :: DataSource' DSNKind -> ProcessT (R.ResourceT IO) (Event ()) (Event Hdon.Notification)
readDSN ds0 = stopped

requireRange :: T -> DataSource -> RPH -> IO ()
requireRange model ((^? _DSSSource) -> Just ds0) rph = fmap (const ()) $ forkIO $
  do
    res <- initialReadDs ds0
    sts <- either (\s -> error ("requireRange error\n" ++ show s)) return res
    let testNoLeft tgt = not . null $ filter (\st -> Hdon.statusId st == tgt) sts
        noLeft = maybe False testNoLeft (rph ^. rphLower)
    mailboxPost (model ^. updateRPHMBox) (rph ^. rphId, sts, noLeft)
  where
    initialReadDs (DataSource _ DSHome) = Hdon.getHomeTimelineWithOption client q
    initialReadDs (DataSource _ (DSUserStatus userId)) =
        Hdon.getAccountStatuses client userId

    initialReadDs _ = Hdon.getPublicTimelineWithOption client q
    client = (ds0 ^. hastodonClient)
    q = mconcat $ catMaybes [
        Hdon.minId <$> (rph ^. rphLower),
        Hdon.maxId <$> (rph ^. rphUpper)
      ]

requireRange model ((^? _DSNSource) -> Just ds0) rph = fmap (const ()) $ forkIO $
  do
    res <- initialReadDs ds0
    sts <- either (\s -> error ("requireRange error\n" ++ show s)) return res
    let testNoLeft tgt = not . null $ filter (\st -> Hdon.notificationId st == tgt) sts
        noLeft = maybe False testNoLeft (rph ^. rphLower)
    mailboxPost (model ^. updateRPHNMBox) (rph ^. rphId, sts, noLeft)
  where
    initialReadDs ds@(DataSource _ DSNotification) = Hdon.getNotificationsWithOption client q
    client = ds0 ^. hastodonClient
    q = mconcat $ catMaybes [
        Hdon.minId <$> (rph ^. rphLower),
        Hdon.maxId <$> (rph ^. rphUpper)
      ]

onUpdateRange model = proc world ->
  do
    onMailboxPost $ model ^. updateRPHMBox -< world

onUpdateNRange model = proc world ->
  do
    onMailboxPost $ model ^. updateRPHNMBox -< world

updateStatus :: T -> Hdon.HastodonClient -> Int -> IO ()
updateStatus model cli i = fmap (const ()) $ forkIO $
  do
    Right st <- Hdon.getStatus cli i
    mailboxPost (model ^. updateStMBox) st
    return ()

onUpdateStatus model = proc world ->
  do
    onMailboxPost $ model ^. updateStMBox -< world

sendFav :: T -> Hdon.HastodonClient -> Int -> IO ()
sendFav model cli i = (() <$) $ forkIO $ (() <$) $ runMaybeT $
  do
    st <- warnIfFail $ Hdon.postFavorite cli i
    liftIO $ mailboxPost (model ^. updateStMBox) st
    return ()

sendReblog :: T -> Hdon.HastodonClient -> Int -> IO ()
sendReblog model cli i = (() <$) $ forkIO $ (() <$) $ runMaybeT $
  do
    st <- warnIfFail $ Hdon.postReblog cli i
    liftIO $ mailboxPost (model ^. updateStMBox) st
    return ()

warnIfFail :: Show e => IO (Either e a) -> MaybeT IO a
warnIfFail m = lift m >>= \case
    Left err -> lift (print err) >> mzero
    Right x -> return x
