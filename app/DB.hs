
module
    DB
where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.FilePath
import System.Directory

import BasicModel

writeReg :: Registration -> IO ()
writeReg reg =
  do
    stgDir <- getAppUserDataDirectory "armageddon"
    let authFile = stgDir </> "auth.splite3"

    fileEx <- doesFileExist authFile
    authConn <-
        if fileEx
            then openAuth authFile
            else createAuth authFile
    disconnect authConn
    return ()

createAuth authFile =
  do
    conn <- connectSqlite3 authFile
    runRaw conn "CREATE TABLE basic(defaultClientName text)"
    runRaw conn "CREATE TABLE host(hostname text, clientId text, clientSecret text);"
    runRaw conn "CREATE TABLE registration(hostname text, username text, token text);"
    commit conn
    return conn

openAuth authFile =
  do
    connectSqlite3 authFile
