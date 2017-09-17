{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

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

import qualified Data.Text as Text
import Data.Maybe (listToMaybe)

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

import Graphics.UI.Gtk (postGUIAsync)
import BasicModel
import qualified DB.Init as DB
import qualified DB.Types as DB

import qualified Web.Hastodon as Hdon

type TheWorld = World IO IO IORefRunner
type TheMBox = Mailbox IO IO IORefRunner


data Unordered a = Add a | Del a | Clear
makePrisms ''Unordered

--
-- Data model type
--
data T = T {
    _regMBox :: TheMBox (Unordered Registration),
    _selDSMBox :: TheMBox DataSource
  }
makeLenses ''T

init ::
    (HasWorld IO IO IORefRunner i, Occasional o) =>
    Evolution i o IO T
init =
  do
    regB <- mailboxNew
    selDSB <- mailboxNew
    return $ T {
        _regMBox = regB,
        _selDSMBox = selDSB
      }

--
-- Internal
--
getAuthConn :: R.ResourceT IO Connection
getAuthConn =
  do
    (_, conn) <- R.allocate DB.prepareAuth disconnect
    return conn

dbReg :: Prism' DB.Registration Registration
dbReg = prism' regTo regFrom
  where
    regTo (Registration hst un tok) =
        DB.Registration (Just hst) (Just un) (Just tok)
    regFrom reg@(DB.Registration hst un tok) =
        Registration <$> hst <*> un <*> tok

dbHost :: Prism' DB.Host Host
dbHost = prism' hostTo hostFrom
  where
    hostTo (Host hn cid cs) =
        DB.Host (Just hn) (Just cid) (Just cs)
    hostFrom h@(DB.Host hn cid cs) =
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
        runQuery conn (relationalQuery DB.registration) ()
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
            wheres $ h ! DB.hostname' .=. value (Just (hst ^. hostname))
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
        h <- query DB.host
        wheres $ h ! DB.hostname' .=. value (Just (Text.pack hn))
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
        liftIO $ postGUIAsync $
            mailboxPost (model ^. regMBox) $ Add reg

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

onSelDS :: T -> ProcessT IO TheWorld (Event DataSource)
onSelDS model = proc world ->
  do
    onMailboxPost $ model ^. selDSMBox -< world

readDS :: DataSource -> ProcessT (R.ResourceT IO) (Event ()) (Event Hdon.Status)
readDS ds0 = constructT $
  do
    Right sts <- liftIO $ initialReadDs ds0
    mapM_ yield $ reverse sts
    auto $ (C.catchC (sourceReadDs ds0) (liftIO . printEx)) C.=$= filterUpdateC
  where
    initialReadDs ds@(DataSource _ DSHome) = Hdon.getHomeTimeline (ds ^. hastodonClient)
    initialReadDs ds = Hdon.getPublicTimeline (ds ^. hastodonClient)

    sourceReadDs ds@(DataSource _ DSHome) = Hdon.sourceUserTimeline (ds ^. hastodonClient)
    sourceReadDs ds = Hdon.sourcePublicTimeline (ds ^. hastodonClient)

    filterUpdateC = C.awaitForever $ \case
        Hdon.StreamUpdate x -> C.yield x
        _ -> return ()

    printEx :: CA.ParseError -> IO ()
    printEx = print

