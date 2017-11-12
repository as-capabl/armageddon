{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module
    DBCommon
where

import Control.Monad (sequence_)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer (execWriterT, tell)

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

defineTable :: FilePath -> String -> Q [Dec]
defineTable filepath tableName =
  do
    defineTableFromDB
        (connectSqlite3 filepath)
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

defineAll :: FilePath -> [String] -> Q [Dec]
defineAll filepath l =
    execWriterT $ sequence_ $ map (liftW . defineTable filepath) l
  where
    liftW mx = lift mx >>= tell
