{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module
    DB.Init
where

import GHC.Generics (Generic)
import Database.HDBC.Query.TH (defineTableFromDB)
import Database.HDBC.Schema.Driver (typeMap)
import Database.HDBC.Schema.SQLite3 (driverSQLite3)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC (runRaw, commit)
import Language.Haskell.TH (Q, Dec, runIO)
import System.FilePath
import System.Directory

import BasicModel (BMText)

defineTable :: FilePath -> String -> Q [Dec]
defineTable fileName tableName =
  do
    conn <- runIO $ prepareAuth
    defineTableFromDB
        (return conn)
        (driverSQLite3 { typeMap = myTypeMap })
        "main" -- schema name, ignored by SQLite
        tableName
        [''Show, ''Generic]
  where
    myTypeMap =
      [
        ("float", [t|Double|]),
        ("integer", [t|Int|]),
        ("text", [t|BMText|])
      ]

prepareAuth =
  do
    stgDir <- getAppUserDataDirectory "armageddon"
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
    runRaw conn "CREATE TABLE file(fileVersion integer, fileKind text)"
    runRaw conn "CREATE TABLE config(configDefaultClientName text)"
    runRaw conn "CREATE TABLE host(hostName text, hostClientId text, hostClientSecret text);"
    runRaw conn "CREATE TABLE registration(registrationHost text, registrationToken text, registrationUser text);"
    commit conn
    return conn

