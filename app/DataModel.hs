{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

-- Global event
module
    DataModel
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Lens hiding (set)
import Control.Concurrent (forkIO)
import Control.Monad (forM)
import Control.Monad.Trans (lift, liftIO)
import qualified Control.Monad.Trans.Resource as R

import Control.Arrow.Machine
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner

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

type TheWorld = World IO IO IORefRunner
type TheMBox = MessageBox IO IO IORefRunner


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
    regB <- messageBoxNew
    selDSB <- messageBoxNew
    return $ T {
        _regMBox = regB,
        _selDSMBox = selDSB
      }

--
-- Internal
--
writeReg :: Registration -> IO ()
writeReg reg = R.runResourceT $
  do
    (_, conn) <- R.allocate DB.prepareAuth disconnect
    liftIO $
      do
        runInsert conn insertReg ()
        commit conn
    return ()
  where
    insertReg = derivedInsertValue $
      do
        DB.reghostname' <-# value (Just $ reg ^. registrationHost)
        DB.username' <-# value (Just $ reg ^. registrationUser)
        DB.token' <-# value (Just $ reg ^. registrationToken)
        return unitPlaceHolder

readRegs :: T -> ProcessT (R.ResourceT IO) (Event ()) (Event ())
readRegs model = readAuth >>> postAuth
  where
    readAuth = constructT $
      do
        (_, conn) <- lift $ R.allocate DB.prepareAuth disconnect
        regs <- liftIO $
            runQuery conn (relationalQuery DB.registration) ()
        forM regs $ \(DB.Registration hst un tok) ->
          do
            mapM_ yield $ Registration <$> hst <*> un <*> tok
        return ()

    postAuth = fire $ \reg ->
        liftIO $ postGUIAsync $
            postMessage (model ^. regMBox) $ Add reg



--
-- Accessors
--
loadSetting :: T -> IO ()
loadSetting model =
  do
    forkIO $ R.runResourceT $ runT_ (readRegs model) (repeat ())
    return ()

addRegistration :: T -> Registration -> IO ()
addRegistration model reg =
  do
    writeReg reg
    postMessage (model ^. regMBox) $ Add reg

onAddReg ::
    T -> ProcessT IO TheWorld (Event Registration)
onAddReg model = proc world ->
  do
    ureg <- onMessage $ model ^. regMBox -< world
    returnA -< filterJust $ (^? _Add) <$> ureg

selDS :: T -> DataSource -> IO ()
selDS model ds =
  do
    postMessage (model ^. selDSMBox) $ ds

onSelDS :: T -> ProcessT IO TheWorld (Event DataSource)
onSelDS model = proc world ->
  do
    onMessage $ model ^. selDSMBox -< world
