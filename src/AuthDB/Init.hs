{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module
    AuthDB.Init
where

import GHC.Generics (Generic)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (Connection, connectSqlite3)
import Database.HDBC (runRaw, commit, disconnect)
import Language.Haskell.TH (Q, Dec, runIO)
import System.FilePath
import System.Directory

import BasicModel (BMText)
import DBCommon


prepareAuth =
  do
    stgDir <- getAppUserDataDirectory "armageddon"
    createDirectoryIfMissing True stgDir
    let authFile = stgDir </> "auth.splite3"

    fileEx <- doesFileExist authFile
    conn <-
        if fileEx
            then connectSqlite3 authFile
            else createAuth authFile
    return conn

createAuth authFile =
  do
    conn <- connectSqlite3 authFile
    runRaw conn "CREATE TABLE file(fileVersion integer primary key, fileKind text);"
    runRaw conn "CREATE TABLE config(configDefaultClientName text primary key);"
    runRaw conn "CREATE TABLE host(hostName text primary key, hostClientId text, hostClientSecret text);"
    runRaw conn "CREATE TABLE registration(registrationHost text, registrationToken text, registrationUser text, primary key(registrationHost, registrationUser));"
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

        conn <- createAuth filepath
        disconnect conn

        return filepath

    r <- defineAll filepath ["file", "config", "host", "registration"]

    runIO $ removePathForcibly filepath
    return r

