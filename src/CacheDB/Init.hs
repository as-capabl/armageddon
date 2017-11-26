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
    runRaw conn "CREATE TABLE host(hostId integer primary key, hostName string);"
    runRaw conn $ "CREATE TABLE status(" ++
        "statusId integer," ++
        "statusUri text," ++
        "statusUrl text," ++
        "statusAccount integer," ++
        "statusInReplyToId integer," ++ -- nullable
        "statusInReplyToAccountId integer," ++ --nullable
        "statusReblog integer," ++ -- nullable
        "statusContent text," ++
        "statusCreatedAt text," ++
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
        "primary key(statusId, statusHostId));"
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

    r <- defineAll filepath ["file", "status"]

    runIO $ removePathForcibly filepath
    return r

