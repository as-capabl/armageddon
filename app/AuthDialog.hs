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
import Control.Lens
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

import BasicModel

getHostname =
  do
    dialog <- dialogNew

    labelHost <- labelNew (Just "Host URL" :: Maybe Text.Text)
    entryHost <- entryNew
    labelApp <- labelNew (Just "App name (to display)" :: Maybe Text.Text)
    entryApp <- entryNew
    entrySetText entryApp ("YourName@armageddon" :: Text.Text)

    vbox <- dialogGetUpper dialog
    vbox `containerAdd` labelHost
    vbox `containerAdd` entryHost
    vbox `containerAdd` labelApp
    vbox `containerAdd` entryApp

    dialogAddButton dialog ("OK" :: Text.Text) ResponseOk
    dialogAddButton dialog ("Cancel" :: Text.Text) ResponseCancel

    widgetShowAll dialog
    stat <- dialogRun dialog
    strHost <- entryGetText entryHost :: IO String
    strApp <- entryGetText entryApp :: IO String
    widgetDestroy dialog

    return $ if stat == ResponseOk then Just (strHost, strApp) else Nothing


authPasswd host clientId clientSecret =
  do
    mauth <- showPassDlg
    maybe (return Nothing) `flip` mauth $ \(user, pass) ->
      do
        mcli <- Hdon.mkHastodonClient clientId clientSecret user pass host
        let makeReg cli =
                Registration "" "" (Text.pack user) & hastodonClient .~ cli
            mreg = makeReg <$> mcli
        maybe (authPasswd host clientId clientSecret) (return . Just) mreg

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
