{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module
    Control.Arrow.Machine.IORefRunner
where

import Control.Monad.Trans.Control
import Control.Monad.Base
import qualified Control.Arrow.Machine.World as Mc
import Data.IORef

data IORefRunner (instr :: * -> *) (m :: * -> *) = IORefRunner

instance
    MonadBaseControl IO m =>
    Mc.WorldRunner IO m (IORefRunner IO m)
  where
    type Ref (IORefRunner IO m) = IORef

    newRef _ = newIORef
    refGet _ = readIORef
    refSet _ = writeIORef
