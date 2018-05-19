{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

-- Global event
module
    DataModel
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Lens hiding (set)
import Control.Concurrent (forkIO)
import Control.Monad (forM, mzero, mplus, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Maybe
import Control.Exception (bracket)
import qualified Control.Monad.Trans.Resource as R

import Control.Arrow.Machine
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner
import Control.Arrow.Machine.ConduitAdaptor

import qualified Data.Text as Text
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (sortOn, nubBy)
import Data.Int (Int64)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Attoparsec as CA

import Database.Relational.Query
import Database.HDBC (commit, disconnect)
import Database.HDBC.Sqlite3
import Database.HDBC.Query.TH
import Database.HDBC.Record
import Database.Record.Persistable
import System.FilePath
import System.Directory
import Data.Time.Format

import BasicModel
import qualified AuthDB.Init as AuthDB
import qualified AuthDB.Types as AuthDB
import qualified CacheDB.Init as CacheDB
import qualified CacheDB.Types as CacheDB

import qualified Web.Hastodon as Hdon

type TheWorld runner = World IO IO runner
type TheMBox runner = Mailbox IO IO runner


data Unordered a = Add a | Del a | Clear
makePrisms ''Unordered

--
-- Data model type
--
data T runner = T {
    _regMBox :: TheMBox runner (Unordered Registration),
    _updateRPHMBox :: TheMBox runner (BMText, [Hdon.Status], Bool),
    _updateRPHNMBox :: TheMBox runner (BMText, [Hdon.Notification], Bool),
    _updateStMBox :: TheMBox runner Hdon.Status,
    _selDSMBox :: TheMBox runner DataSource
  }
makeLenses ''T

init ::
    (WorldRunner IO IO (runner IO IO), HasWorld IO IO runner i, Occasional o) =>
    Evolution i o IO (T runner)
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

getCacheConn :: R.ResourceT IO Connection
getCacheConn =
  do
    (_, conn) <- R.allocate CacheDB.prepareCache disconnect
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

type HostCacheId = Int
chdbAccount :: Prism' CacheDB.Account (Hdon.Account, HostCacheId)
chdbAccount = prism' accountTo accountFrom
  where
    accountTo (Hdon.Account {..}, hostId) =
        CacheDB.Account
          {
            accountid = toS accountId,
            accountusername = toS accountUsername,
            accountacct = toS accountAcct,
            accountdisplayname = toS accountDisplayName,
            accountlocked = toB accountLocked,
            accountcreatedat = toTime accountCreatedAt,
            accountfollowerscount = toI accountFollowersCount,
            accountfollowingcount = toI accountFollowingCount,
            accountstatusescount = toI accountStatusesCount,
            accountnote = toS accountNote,
            accounturl = toS accountUrl,
            accountavatar = toS accountAvatar,
            accountavatarstatic = toS accountAvatarStatic,
            accountheader = toS accountHeader,
            accountheaderstatic = toS accountHeaderStatic,
            accounthostid = toI hostId
          }
    accountFrom (CacheDB.Account {..}) = (,) <$> acc <*> fromI accounthostid
      where
        acc =
          do
            accountId <- fromS accountid
            accountUsername <- fromS accountusername
            accountAcct <- fromS accountacct
            accountDisplayName <- fromS accountdisplayname
            accountLocked <- fromB accountlocked
            accountCreatedAt <- fromTime accountcreatedat
            accountFollowersCount <- fromI accountfollowerscount
            accountFollowingCount <- fromI accountfollowingcount
            accountStatusesCount <- fromI accountstatusescount
            accountNote <- fromS accountnote
            accountUrl <- fromS accounturl
            accountAvatar <- fromS accountavatar
            accountAvatarStatic <- fromS accountavatarstatic
            accountHeader <- fromS accountheader
            accountHeaderStatic <- fromS accountheaderstatic
            return Hdon.Account{..}
    toI = Just . fromIntegral
    fromI = fmap fromIntegral
    toS = Just . Text.pack
    fromS = fmap Text.unpack
    toTime str = toDBTime <$> parseTimeM True defaultTimeLocale isoTimeFormat str
    fromTime = fmap (formatTime defaultTimeLocale isoTimeFormat . fromDBTime)
    toB x = Just $ if x then 1 else 0
    fromB = fmap (/= 0)

chdbStatus ::
    Prism'
        (CacheDB.Status, CacheDB.Account, Maybe (CacheDB.Status, CacheDB.Account))
        (Hdon.Status, HostCacheId, Maybe DSKind)
chdbStatus = prism' statusTo statusFrom
  where
    statusTo (Hdon.Status {..}, hostId, dsKind) = (st, acc, rebl)
      where
        st = CacheDB.Status
          {
            statusid = toS statusId,
            statusuri = toS statusUri,
            statusurl = toS statusUrl,
            statusaccount = toS $ Hdon.accountId statusAccount,
            statusinreplytoid = statusInReplyToId >>= toS,
            statusinreplytoaccountid = statusInReplyToAccountId >>= toS,
            statusreblog = fmap Hdon.statusId statusReblog >>= toS,
            statuscontent = toS statusContent,
            statuscreatedat = toTime statusCreatedAt,
            statusreblogscount = toI statusReblogsCount,
            statusfavouritescount = toI statusFavouritesCount,
            statusreblogged = toMB statusReblogged,
            statusfavourited = toMB statusFavourited,
            statussensitive = toMB statusSensitive,
            statusspoilertext = toS statusSpoilerText,
            statusvisibility = toS statusVisibility,
            statushostid = toI hostId,
            statusdskind = Text.pack . show <$> dsKind
          }
        acc =
            (statusAccount, hostId) ^. re chdbAccount
        rebl =
          do
            x <- statusReblog
            let (s, a, _) = (x, hostId, Nothing) ^. re chdbStatus
            return (s, a)

    statusFrom (CacheDB.Status {..}, acc, rebl) =
        (,,) <$> stBody <*>
            fromI statushostid <*> (pure $ statusdskind >>= (^? _Show) . Text.unpack)
      where
        stBody =
          do
            statusId <- fromS statusid
            statusUri <- fromS statusuri
            statusUrl <- fromS statusurl
            statusAccount <- fst <$> (acc ^? chdbAccount)
            statusInReplyToId <- pure $ fromS statusinreplytoid
            statusInReplyToAccountId <- pure $ fromS statusinreplytoaccountid
            statusReblog <- pure $
              do
                (s, a) <- rebl
                (chs, _, _) <- (s, a, Nothing) ^? chdbStatus
                return chs
            statusContent <- fromS statuscontent
            statusCreatedAt <- fromTime statuscreatedat
            statusReblogsCount <- fromI statusreblogscount
            statusFavouritesCount <- fromI statusfavouritescount
            statusReblogged <- fromMB statusreblogged
            statusFavourited <- fromMB statusfavourited
            statusSensitive <- fromMB statussensitive
            statusSpoilerText <- fromS statusspoilertext
            statusVisibility <- fromS statusvisibility
            statusMediaAttachments <- pure []
            statusMentions <- pure []
            statusTags <- pure []
            statusApplication <- pure Nothing
            return Hdon.Status{..}
    toI = Just . fromIntegral
    fromI = fmap fromIntegral
    toS = Just . Text.pack
    fromS = fmap Text.unpack
    toTime str = toDBTime <$> parseTimeM True defaultTimeLocale isoTimeFormat str
    fromTime = fmap (formatTime defaultTimeLocale isoTimeFormat . fromDBTime)
    toB x = Just $ if x then 1 else 0
    fromB = fmap (/= 0)
    toMB mx = mx >>= toB
    fromMB = Just . fromB

writeReg :: Registration -> IO ()
writeReg reg = R.runResourceT $
  do
    conn <- getAuthConn
    lift $ runInsert conn (derivedInsert id') (reg ^. re dbReg)
    lift $ commit conn

readRegs :: T runner -> ProcessT (R.ResourceT IO) (Event ()) (Event Registration)
readRegs model = constructT $
  do
    conn <- lift $ getAuthConn
    regs <- liftIO $
        runQuery conn (relationalQuery AuthDB.registration) ()
    liftIO $ commit conn
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

writeStatusCache :: Hostname -> DSKind -> [Hdon.Status] -> IO ()
writeStatusCache hn dsKind sts = when (isCachableDS dsKind) $ R.runResourceT $
  do
    conn <- getCacheConn

    -- Get or register host id.
    hstGot <- getHost conn
    hst <- maybe (writeHostAndGetId conn) return hstGot

    -- Write
    (addSts, addAccs, updAccs) <- splitSts conn hst
    liftIO $
      do
        mapM_ (runInsert conn (derivedInsert id')) addSts
        mapM_ (runInsert conn (derivedInsert id')) addAccs
        commit conn

  where
    getHost conn = liftIO $
      do
        hs <- runQuery conn `flip` () $ relationalQuery . relation $
          do
            h <- query CacheDB.host
            wheres $ h ! CacheDB.hostname' .=. value (Just hn)
            return h
        return $ case hs
          of
            (x:_) -> fromIntegral <$> CacheDB.hostid x
            _ -> Nothing

    writeHostAndGetId conn = liftIO $
      do
        h <- runInsert conn `flip` () $ derivedInsertValue $
          do
            -- Primary key is auto set and `runInsert` returns it.
            CacheDB.hostname' <-# value (Just hn)
            return unitPlaceHolder
        return $ fromIntegral h


    splitSts conn hostId = liftIO $
      do
        let spl st = (st, hostId, Just dsKind) ^. re chdbStatus
            l = spl <$> sts
            dbSts = l >>= \(dbSt, _, mDbRebl) -> [dbSt] ++ maybe [] (return . fst) mDbRebl
            dbAccs = l >>= \(_, dbAcc, mDbRebl) -> [dbAcc] ++ maybe [] (return . snd) mDbRebl
        (addSts, _) <- divideByDB conn dbSts CacheDB.statusid CacheDB.status CacheDB.statusid'
        (addAccs, updAccs) <- divideByDB conn dbAccs CacheDB.accountid CacheDB.account CacheDB.accountid'
        return (addSts, addAccs, updAccs)

    divideByDB ::
        PersistableWidth a =>
        Connection -> [a] -> (a -> Maybe BMText) -> Relation () a -> Pi a (Maybe BMText) ->
        IO ([a], [a])
    divideByDB conn l0 pickId record fldId =
      do
        let l = nubBy (\x y -> pickId x == pickId y) $ sortOn pickId l0
            pickedIds = catMaybes (pickId <$> l)

        -- Search DB
        preExistingIds <- liftIO $ runQuery conn `flip` () $ relationalQuery . relation $
          do
            h <- query record
            wheres $ h ! fldId `in'` values (Just <$> pickedIds)
            asc $ h ! fldId
            return $ h ! fldId
        return $ divideByMerge pickId l preExistingIds

    divideByMerge _ [] _ = ([], [])
    divideByMerge _ l [] = (l, [])
    divideByMerge f l@(x:xs) l2@(y:ys)
        | f x == y = let (r1, r2) = divideByMerge f xs l2 in (r1, x:r2)
        | f x < y = let (r1, r2) = divideByMerge f xs l2 in (x:r1, r2)
        | f x > y = divideByMerge f l ys

{-
        maybe (return ()) `flip` mDbRebl $ \(dbReblSt, dbReblAcc) ->
          do
            runInsert conn (derivedInsert id') dbReblSt
            runInsert conn (derivedInsert id') dbReblAcc
            return ()
-}

readStatusCache :: Hostname -> DSKind -> IO [Hdon.Status]
readStatusCache hn dsKind
    | isCachableDS dsKind = R.runResourceT $
      do
        conn <- getCacheConn

        sts <- liftIO $ runQuery conn `flip` () $ relationalQuery . relation $
          do
            st <- query CacheDB.status
            acc <- query CacheDB.account
            on $ st ! CacheDB.statusaccount' .=. acc ! CacheDB.accountid'

            wheres $ st ! CacheDB.statusdskind' .=. value (Just $ Text.pack $ show dsKind)
            desc $ st ! CacheDB.statusid'
            return $ (,) |$| st |*| acc
        liftIO $ commit conn
        return $
          do
            (dbSt, dbAcc) <- sts
            (st, _, _) <- maybe mzero return $ (dbSt, dbAcc, Nothing) ^? chdbStatus
            return st

    | otherwise =
        return []


--
-- Accessors
--
loadSetting :: WorldRunner IO IO (runner IO IO) => T runner -> IO ()
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

addRegistration :: WorldRunner IO IO (runner IO IO) => T runner -> Registration -> IO ()
addRegistration model reg =
  do
    writeReg reg
    mailboxPost (model ^. regMBox) $ Add reg

onAddReg ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> ProcessT IO (TheWorld runner) (Event Registration)
onAddReg model = proc world ->
  do
    ureg <- onMailboxPost $ model ^. regMBox -< world
    returnA -< filterJust $ (^? _Add) <$> ureg

selDS ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> DataSource -> IO ()
selDS model ds =
  do
    mailboxPost (model ^. selDSMBox) $ ds

selUserDSByStatusId ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> Registration -> Hdon.HastodonId -> IO ()
selUserDSByStatusId model reg statusId = fmap (const ()) $ forkIO $
  do
    Right st <- Hdon.getStatus (reg ^. hastodonClient) statusId
    let account = Hdon.statusAccount st
        accountId = Hdon.accountId account
    selDS model $ DataSource reg (DSS $ DSUserStatus (Text.pack accountId))

onSelDS ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> ProcessT IO (TheWorld runner) (Event DataSource)
onSelDS model = proc world ->
  do
    onMailboxPost $ model ^. selDSMBox -< world

readDSS ::
    WorldRunner IO IO (runner IO IO) =>
    DataSource' DSSKind -> TheMBox runner Hdon.Status -> IO ()
readDSS ds0 mb = (() <$) $ forkIO $ R.runResourceT $
  do
    C.runConduit $ (C.catchC (sourceReadDs ds0) (liftIO . printEx)) C.=$= filterUpdateC
  where
    sourceReadDs ds@(DataSource _ DSHome) = Hdon.sourceUserTimeline (ds ^. hastodonClient)
    sourceReadDs (DataSource _ (DSUserStatus _)) = return ()
    sourceReadDs ds = Hdon.sourcePublicTimeline (ds ^. hastodonClient)

    filterUpdateC = C.awaitForever $ \case
        Hdon.StreamUpdate x -> liftIO $ mailboxPost mb x
        _ -> return ()

    printEx :: CA.ParseError -> IO ()
    printEx = print

readDSN :: DataSource' DSNKind -> ProcessT (R.ResourceT IO) (Event ()) (Event Hdon.Notification)
readDSN ds0 = stopped

requireRange ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> DataSource -> RPH -> IO ()
requireRange model ((^? _DSSSource) -> Just ds0) rph = fmap (const ()) $ forkIO $
  do
    -- Fetch by cache
    sts0 <- readStatusCache (Text.pack $ ds0 ^. host) (DSS $ _dsKind ds0)
    mailboxPost (model ^. updateRPHMBox) (rph ^. rphId, sts0, False)

    -- Fetch by HTTP
    res <- initialReadDs ds0
    sts <- either (\s -> error ("requireRange error\n" ++ show s)) return res
    let testNoLeft tgt = not . null $ filter (\st -> Hdon.statusId st == show tgt) sts
        noLeft = maybe False testNoLeft (rph ^. rphLower)
    mailboxPost (model ^. updateRPHMBox) (rph ^. rphId, sts, noLeft)

    -- Cache
    let hst = Text.pack $ ds0 ^. host
        kin = DSS $ _dsKind ds0
    writeStatusCache hst kin sts
  where
    initialReadDs (DataSource _ DSHome) =
        Hdon.getHomeTimelineWithOption client q
    initialReadDs (DataSource _ (DSUserStatus userId)) =
        Hdon.getAccountStatuses client (Text.unpack userId)
    initialReadDs _ =
        Hdon.getPublicTimelineWithOption client q

    client = (ds0 ^. hastodonClient)
    q = mconcat $ catMaybes [
        Hdon.minId . show <$> (rph ^. rphLower),
        Hdon.maxId . show <$> (rph ^. rphUpper)
      ]

requireRange model ((^? _DSNSource) -> Just ds0) rph = fmap (const ()) $ forkIO $
  do
    res <- initialReadDs ds0
    sts <- either (\s -> error ("requireRange error\n" ++ show s)) return res
    let testNoLeft tgt = not . null $ filter (\st -> Hdon.notificationId st == show tgt) sts
        noLeft = maybe False testNoLeft (rph ^. rphLower)
    mailboxPost (model ^. updateRPHNMBox) (rph ^. rphId, sts, noLeft)
  where
    initialReadDs ds@(DataSource _ DSNotification) = Hdon.getNotificationsWithOption client q
    client = ds0 ^. hastodonClient
    q = mconcat $ catMaybes [
        Hdon.minId . show <$> (rph ^. rphLower),
        Hdon.maxId . show <$> (rph ^. rphUpper)
      ]

onUpdateRange model = proc world ->
  do
    onMailboxPost $ model ^. updateRPHMBox -< world

onUpdateNRange model = proc world ->
  do
    onMailboxPost $ model ^. updateRPHNMBox -< world

updateStatus ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> Hdon.HastodonClient -> Hdon.HastodonId -> IO ()
updateStatus model cli i = fmap (const ()) $ forkIO $
  do
    Right st <- Hdon.getStatus cli i
    mailboxPost (model ^. updateStMBox) st
    return ()

onUpdateStatus model = proc world ->
  do
    onMailboxPost $ model ^. updateStMBox -< world

sendFav ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> Hdon.HastodonClient -> Hdon.HastodonId -> IO ()
sendFav model cli i = (() <$) $ forkIO $ (() <$) $ runMaybeT $
  do
    st <- warnIfFail $ Hdon.postFavorite cli i
    liftIO $ mailboxPost (model ^. updateStMBox) st
    return ()

sendReblog ::
    WorldRunner IO IO (runner IO IO) =>
    T runner -> Hdon.HastodonClient -> Hdon.HastodonId -> IO ()
sendReblog model cli i = (() <$) $ forkIO $ (() <$) $ runMaybeT $
  do
    st <- warnIfFail $ Hdon.postReblog cli i
    liftIO $ mailboxPost (model ^. updateStMBox) st
    return ()

warnIfFail :: Show e => IO (Either e a) -> MaybeT IO a
warnIfFail m = lift m >>= \case
    Left err -> lift (print err) >> mzero
    Right x -> return x
