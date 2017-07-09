{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}

module
    MainForm
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Void
import Data.Tree
import Control.Monad (mzero)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Text as Text

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.ModelView.TreeStore
import Graphics.UI.Gtk.ModelView.TreeView

import Control.Arrow.Machine
import qualified Graphics.UI.McGtk as Mg
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner

import qualified Web.Hastodon as Hdon

import BasicModel

data PostBox = PostBox {
    postText :: TextBuffer,
    postTextView :: TextView,
    postButton :: Button
  }

data TreePane = TreePane {
    trModel :: TreeStore Text.Text,
    trView :: TreeView,
    trSel :: TreeSelection
  }

data MainForm = MainForm {
    mfWindow :: Window,
    mfPostBox :: PostBox,
    mfTreePane :: TreePane,
    mfTootPane :: WebView
  }

setup :: Text.Text -> IO MainForm
setup html =
  do
    window <- windowNew
    windowSetDefaultSize window 800 600
    windowSetPosition window WinPosCenter

    (mdPost, wPost) <- setupPostBox
    (mdTree, wTree) <- setupTreePane
    (mdToot, wToot) <- setupTootPane html

    -- Pane composition
    paned <- hPanedNew
    paned `panedAdd1` wTree
    paned `panedAdd2` wToot

    -- Total composition
    vbox <- vBoxNew False 5
    boxPackStart vbox wPost PackNatural 0
    boxPackStart vbox paned PackGrow 0
    window `containerAdd` vbox

    widgetShowAll window

    return $ MainForm {
        mfWindow = window,
        mfPostBox = mdPost,
        mfTreePane = mdTree,
        mfTootPane = mdToot
      }

setupPostBox =
  do
    textView <- textViewNew
    textModel <- textViewGetBuffer textView
    {-
    runMaybeT $
      do
        textWin <- MaybeT $ textViewGetWindow textView TextWindowWidget
        lift $ windowSetDefaultSize textWin 100 (-1)
    -}

    button <- buttonNew
    buttonSetLabel button ("Toot" :: Text.Text)

    btnAlign <- alignmentNew 0.5 0.5 0 0
    btnAlign `containerAdd` button

    hbox <- hBoxNew False 5
    boxPackStart hbox textView PackGrow 0
    boxPackStart hbox btnAlign PackNatural 0

    return (PostBox textModel textView button, hbox)

setupTreePane =
  do
    treeModel <- treeStoreNew
      [
        Node "pawoo.net"
          [
            Node "public" [],
            Node "home" []
            --, Node "notification" []
          ]
      ]
    treeView <- treeViewNewWithModel treeModel
    renderer <- cellRendererTextNew

    col <- treeViewColumnNew
    treeViewColumnSetTitle col ("Instances" :: Text.Text)
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer treeModel (\ind -> [cellText := ind])

    treeViewAppendColumn treeView col

    treeSel <- treeViewGetSelection treeView

    return (TreePane treeModel treeView treeSel, treeView)

setupTootPane html =
  do
    webView <- webViewNew
    webSettings <- webSettingsNew
    set webSettings [webSettingsEnableScripts := False]
    webViewSetWebSettings webView webSettings

    scrollWin <- scrolledWindowNew Nothing Nothing
    scrollWin `containerAdd` webView

    webViewLoadString
        webView
        html
        (Just "text/html")
        "about:armageddon"

    return (webView, scrollWin)

getDataSource :: TreePane -> IO (Maybe DataSource)
getDataSource tp = runMaybeT $
  do
    rows <- lift $ treeSelectionGetSelectedRows (trSel tp)
    path <- foldr (\x _ -> return x) mzero rows
    str <- lift $ treeStoreGetValue (trModel tp) path
    kind <- if
        | str == "home" -> return DSHome
        | str == "public" -> return DSPublic
        | str == "notification" -> return DSNotification
        | otherwise -> return DSHome
    return $ DataSource "" "" kind
