{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict, StrictData #-}

module
    AuthDialog
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Void
import Data.Tree
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Text as Text

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.ModelView.TreeStore
import Graphics.UI.Gtk.ModelView.TreeView

import Control.Arrow.Machine
import qualified Graphics.UI.McGtk as Mg
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner

import qualified Web.Hastodon as Hdon


showDialog host clientId clientSecret =
  do
    mauth <- showPassDlg
    maybe (return Nothing) `flip` mauth $ \(user, pass) ->
      do
        mcli <- Hdon.mkHastodonClient clientId clientSecret user pass host
        maybe (showDialog host clientId clientSecret) (return . Just) mcli

  where
    showPassDlg =
      do
        dialog <- dialogNew

        labelHost <- labelNew (Just host)
        entryUser <- entryNew
        entryPass <- entryNew
        entrySetVisibility entryPass False

        vbox <- dialogGetUpper dialog
        vbox `containerAdd` labelHost
        vbox `containerAdd` entryUser
        vbox `containerAdd` entryPass

        dialogAddButton dialog ("OK" :: Text.Text) ResponseOk
        dialogAddButton dialog ("Cancel" :: Text.Text) ResponseCancel

        widgetShowAll dialog
        stat <- dialogRun dialog
        user <- entryGetText entryUser :: IO String
        pass <- entryGetText entryPass :: IO String
        widgetDestroy dialog

        return $ if stat == ResponseOk then Just (user, pass) else Nothing
