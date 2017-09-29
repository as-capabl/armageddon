{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module
    Graphics.UI.McWebkit
      (
        -- * Event handling
        on,
        onSelector
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
import Control.Monad.Trans.Maybe
import Control.Monad.Base
import Control.Monad (guard)
import qualified Control.Arrow.Machine as Mc
import qualified Control.Arrow.Machine.World as Mc
import qualified Graphics.UI.Gtk.WebKit.DOM.Event as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.EventM as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.EventTarget as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as DOM
import qualified System.Glib as Glib
import Data.IORef
import Data.Void
import Unsafe.Coerce
import Debug.Trace



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


on ::
    (Mc.WorldRunner IO m (wr IO m), Monad m, MonadIO m,
        DOM.EventClass e, DOM.EventTargetClass self) =>
    self ->
    DOM.EventName self e ->
    DOM.EventM self e r ->
    Mc.ProcessT m (Mc.World IO m wr) (Mc.Event r)
on self eventName eventM =
    Mc.listen
        (\handler -> liftBase $ DOM.on self eventName (eventM >>= lift . handler))
        (\dispose -> liftBase $ dispose)

onSelector ::
    (Mc.WorldRunner IO m (wr IO m), Monad m, MonadIO m,
        DOM.EventClass e, DOM.EventTargetClass self, Glib.GlibString string) =>
    self ->
    DOM.EventName self e ->
    string ->
    (DOM.Element -> DOM.EventM self e r) ->
    Mc.ProcessT m (Mc.World IO m wr) (Mc.Event r)
onSelector self eventName selector fEvM =
    Mc.listen
        (\handler -> liftBase $ DOM.on self eventName (filterAndGo handler >> return ()))
        (\dispose -> liftBase $ dispose)
  where
    filterAndGo handler = runMaybeT $
      do
        Element elem <- MaybeT $ DOM.eventTarget

        -- Match test for selector
        bMatch <- lift $ DOM.webkitMatchesSelector elem selector
        guard $ bMatch

        -- execute
        lift $ fEvM elem >>= lift . handler

pattern Element :: Glib.GObjectClass obj => DOM.Element -> obj
pattern Element elem <-
    (DOM.castToElement &&& (`Glib.isA` DOM.gTypeElement) -> (elem, True))

