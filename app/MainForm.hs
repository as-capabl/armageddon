{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict, StrictData #-}

module
    MainForm
where

import Prelude hiding ((.), id)

import Data.Void
import Data.Tree
import Control.Category
import Control.Arrow
import Control.Lens hiding (set)
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
    _postText :: TextBuffer,
    _postTextView :: TextView,
    _postButton :: Button
  }

makeClassy ''PostBox

data InstPane = InstPane {
    _instModel :: TreeStore Text.Text,
    _instView :: TreeView,
    _instSel :: TreeSelection,
    _instAddBtn :: Button,
    _instMenuBtn :: Button
  }

makeClassy ''InstPane

data StatusPane = StatusPane {
    _statusView :: WebView
  }

makeClassy ''StatusPane

data T = T {
    _win :: Window,
    _pb :: PostBox,
    _ip :: InstPane,
    _sp :: StatusPane
  }

makeLenses ''T

instance HasPostBox T
  where
    postBox = pb

instance HasInstPane T
  where
    instPane = ip

instance HasStatusPane T
  where
    statusPane = sp


setup :: Text.Text -> IO T
setup html =
  do
    window <- windowNew
    windowSetDefaultSize window 800 600
    windowSetPosition window WinPosCenter

    (mdPost, wPost) <- setupPostBox
    (mdTree, wTree) <- setupInstPane
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

    return $ T window mdPost mdTree mdToot

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

setupInstPane =
  do
    -- Tree view
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

    -- Buttons
    addBtn <- buttonNew
    buttonSetLabel addBtn ("+" :: Text.Text)
    menuBtn <- buttonNew
    buttonSetLabel menuBtn ("=" :: Text.Text)

    hboxBtn <- hBoxNew False 5
    boxPackStart hboxBtn addBtn PackNatural 0
    boxPackEnd hboxBtn menuBtn PackNatural 0

    -- Packing
    vbox <- vBoxNew False 5
    boxPackStart vbox hboxBtn PackNatural 0
    boxPackStart vbox treeView PackGrow 0

    return (InstPane treeModel treeView treeSel addBtn menuBtn, vbox)

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

    return (StatusPane webView, scrollWin)

getDataSource :: HasInstPane self => self -> IO (Maybe DataSource)
getDataSource tp = runMaybeT $
  do
    rows <- lift $ treeSelectionGetSelectedRows (tp ^. instSel)
    path <- foldr (\x _ -> return x) mzero rows
    str <- lift $ treeStoreGetValue (tp ^. instModel) path
    kind <- if
        | str == "home" -> return DSHome
        | str == "public" -> return DSPublic
        | str == "notification" -> return DSNotification
        | otherwise -> return DSHome
    return $ DataSource "" "" kind
