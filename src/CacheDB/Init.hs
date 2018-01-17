module
    CacheDB.Init
where

import GHC.Generics (Generic)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC (runRaw, commit, disconnect)
import Language.Haskell.TH (Q, Dec, runIO)
import System.FilePath
import System.Directory

import BasicModel (BMText)
import DBCommon

prepareCache =
  do
    stgDir <- getAppUserDataDirectory "armageddon"
    createDirectoryIfMissing True stgDir
    let cacheFile = stgDir </> "cache.splite3"

    fileEx <- doesFileExist cacheFile
    conn <-
        if fileEx
            then connectSqlite3 cacheFile
            else createCache cacheFile
    return conn

createCache cacheFile =
  do
    conn <- connectSqlite3 cacheFile
    runRaw conn "CREATE TABLE file(fileVersion integer primary key, fileKind text);"
    runRaw conn "CREATE TABLE host(hostId integer primary key, hostName text);"
    runRaw conn $ "CREATE TABLE status(" ++
        "statusId integer," ++
        "statusUri text," ++
        "statusUrl text," ++
        "statusAccount integer," ++
        "statusInReplyToId integer," ++ -- nullable
        "statusInReplyToAccountId integer," ++ --nullable
        "statusReblog integer," ++ -- nullable
        "statusContent text," ++
        "statusCreatedAt integer," ++ -- time
        "statusReblogsCount integer," ++
        "statusFavouritesCount integer," ++
        "statusReblogged integer," ++ -- boolean
        "statusFavourited integer," ++ -- boolean
        "statusSensitive integer," ++ -- boolean
        "statusSpoilerText text," ++
        "statusVisibility text," ++
        -- "statusMediaAttachments [Attachment]," ++
        -- "statusMentions [Mention]," ++
        -- "statusTags [Tag]," ++
        -- "statusApplication Maybe Application" ++
        "statusHostId integer," ++
        "statusDsKind text," ++ -- show DSKind, nullable
        "primary key(statusId, statusHostId));"

    runRaw conn $ "CREATE TABLE account(" ++
        "accountId integer," ++
        "accountUsername text," ++
        "accountAcct text," ++
        "accountDisplayName text," ++
        "accountLocked integer," ++ -- boolean
        "accountCreatedAt integer," ++ -- time
        "accountFollowersCount integer," ++
        "accountFollowingCount integer," ++
        "accountStatusesCount integer," ++
        "accountNote text," ++
        "accountUrl text," ++
        "accountAvatar text," ++
        "accountAvatarStatic text," ++
        "accountHeader text," ++
        "accountHeaderStatic text," ++
        "accountHostId integer," ++
        "primary key(accountId, accountHostId))"
    commit conn
    return conn

defineTypes =
  do
    filepath <- runIO $
      do
        tmpDir <- getTemporaryDirectory >>= \dir ->
            return $ dir </> "armageddon"
        createDirectoryIfMissing True tmpDir

        let filepath = tmpDir </> "cache.splite3"
        removePathForcibly filepath

        conn <- createCache filepath
        disconnect conn

        return filepath

    r <- defineAll filepath ["file", "host", "status", "account"]

    runIO $ removePathForcibly filepath
    return r

