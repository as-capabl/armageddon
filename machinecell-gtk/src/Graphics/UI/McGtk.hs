{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module
    Graphics.UI.McGtk
      (
        -- * Event handling
        on,
        Replying(..),
        ReplyingV(..),
        Looking'(..),
        looking,

        -- * Running
        gtkReactimate,

        -- * Utility
        onClicked
      )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Control.Monad.Trans.Control
import Control.Monad.Base
import qualified Control.Arrow.Machine as Mc
import qualified Control.Arrow.Machine.World as Mc
import qualified Graphics.UI.Gtk as Gtk
import Data.IORef
import Data.Void
import Unsafe.Coerce



class
    SignalDefault a
  where
    signalDefault :: a

instance
    SignalDefault ()
  where
    signalDefault = ()

instance
    SignalDefault Bool
  where
    signalDefault = True

class
    SignalSource obj arg ret mhnd h |
      h -> obj, h -> arg, h -> ret, h -> mhnd
  where
    reg :: h -> obj -> (arg -> mhnd ret) -> IO (IO ())


instance
    (Gtk.GObjectClass obj, SignalDefault ret) =>
    SignalSource obj arg ret IO
        (Gtk.Signal obj (arg -> IO ret))
  where
    reg sig obj handler =
      do
        cid <- Gtk.on obj sig handler
        return (Gtk.signalDisconnect cid)

instance
    (Gtk.GObjectClass obj, SignalDefault ret) =>
    SignalSource obj () ret IO
        (Gtk.Signal obj (IO ret))
  where
    reg sig obj handler =
      do
        cid <- Gtk.on obj sig (handler ())
        return (Gtk.signalDisconnect cid)

instance
    (Gtk.GObjectClass obj, SignalDefault ret) =>
    SignalSource obj () ret (Gtk.EventM e)
        (Gtk.Signal obj (Gtk.EventM e ret))
  where
    reg sig obj handler =
      do
        cid <- Gtk.on obj sig $ handler ()
        return (Gtk.signalDisconnect cid)

on ::
    (SignalSource obj arg ret mhnd sig, SignalDefault ret,
     Mc.WorldRunner IO m (wr IO m), Monad m, MonadIO mhnd) =>
    obj -> sig -> Mc.ProcessT m (Mc.World IO m wr) (Mc.Event arg)
on obj sig =
    Mc.listen
        (\handler -> liftBase $ reg sig obj (\arg -> liftIO (handler arg) >> return signalDefault))
        (\dispose -> liftBase $ dispose)

infixl 0 `on`


data Replying obj arg ret (mhnd :: * -> *) sig = Replying sig ret

instance
    (SignalSource obj arg ret mhnd sig, Monad mhnd) =>
    SignalSource obj arg () mhnd
        (Replying obj arg ret mhnd sig)
  where
    reg (Replying sig ret) obj handler =
        reg sig obj (\x -> handler x >> return ret)

infixl 1 `Replying`


data ReplyingV obj arg ret (mhnd :: * -> *) sig = ReplyingV sig

instance
    (SignalSource obj arg ret mhnd sig, Monad mhnd, MonadIO mhnd) =>
    SignalSource obj (ret -> mhnd (), arg) () mhnd
        (ReplyingV obj arg ret mhnd sig)
  where
    reg (ReplyingV sig) obj handler = reg sig obj handler'
      where
        handler' x =
          do
            v <- liftIO $ newEmptyMVar
            handler (liftIO . putMVar v, x)
            liftIO $ takeMVar v


data Looking' obj arg ret (mhnd :: * -> *) val sig = Looking' sig (arg -> mhnd val)

instance
    (SignalSource obj arg ret mhnd sig, Monad mhnd, MonadIO mhnd) =>
    SignalSource obj val ret mhnd
        (Looking' obj arg ret mhnd val sig)
  where
    reg (Looking' sig mval) obj handler = reg sig obj (\x -> mval x >>= handler)

infixl 1 `Looking'`

looking sig mval = Looking' sig (const mval)

infixl 1 `looking`

-- |Actuate an event handling process.
gtkReactimate ::
    (Mc.WorldRunner IO m (wr IO m), MonadBaseControl IO m, Monad m) =>
    Mc.ProcessT m (Mc.World IO m wr) (Mc.Event Void) ->
    m ()
gtkReactimate sf =
  do
    liftBase Gtk.initGUI
    Mc.start (liftBase Gtk.mainQuit) sf
    liftBase Gtk.mainGUI

--
-- Utility
--

onClicked ::
    (Mc.WorldRunner IO m (wr IO m), Monad m, Gtk.WidgetClass self) =>
    self -> Mc.ProcessT m (Mc.World IO m wr) (Mc.Event ())
onClicked w = proc world ->
  do
    mouse <-
        w `on` Gtk.buttonPressEvent
            `looking` ((,) <$> Gtk.eventClick <*> Gtk.eventButton)
                -< world
    click <- Mc.filterEvent (== (Gtk.SingleClick, Gtk.LeftButton)) -< mouse
    returnA -< Mc.collapse click

