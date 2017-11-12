{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict, StrictData #-}

module
    MainForm
where

import Prelude hiding ((.), id)

import Data.Void
import Data.Tree
import Data.Maybe (isNothing)
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

--
-- Item of ListView (private)
--
data DSItem = DsiReg Registration | DsiDs DataSource deriving (Eq, Show)

makeDsiTree x =
    Node (DsiReg x)
      [
        Node (DsiDs (DataSource x (DSS DSHome))) [],
        Node (DsiDs (DataSource x (DSN DSNotification))) [],
        Node (DsiDs (DataSource x (DSS DSPublic))) []
        --, Node "notification" []
      ]

dsiLabel (DsiReg reg) = reg ^. host
dsiLabel (DsiDs (DataSource _ (DSS DSHome))) = "home"
dsiLabel (DsiDs (DataSource _ (DSS DSPublic))) = "public"
dsiLabel (DsiDs (DataSource _ (DSN DSNotification))) = "notifications"

getDs (DsiReg reg) = DataSource reg (DSS DSHome)
getDs (DsiDs ds) = ds

--
-- GUI objects
--
data PostBox = PostBox {
    _postText :: TextBuffer,
    _postTextView :: TextView,
    _postButton :: Button,
    _postDst :: ListStore Registration,
    _postDstCombo :: ComboBox
  }

makeClassy ''PostBox

data InstPane = InstPane {
    _instModel :: TreeStore DSItem,
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
instance HasPostBox T where {postBox = pb}
instance HasInstPane T where {instPane = ip}
instance HasStatusPane T where {statusPane = sp}

--
-- Construction
--
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
    -- Text area
    textView <- textViewNew
    textModel <- textViewGetBuffer textView
    {-
    runMaybeT $
      do
        textWin <- MaybeT $ textViewGetWindow textView TextWindowWidget
        lift $ windowSetDefaultSize textWin 100 (-1)
    -}

    -- Button
    button <- buttonNew
    buttonSetLabel button ("Toot" :: Text.Text)

    btnAlign <- alignmentNew 0.5 0.5 0 0
    btnAlign `containerAdd` button

    hbox <- hBoxNew False 5
    boxPackStart hbox textView PackGrow 0
    boxPackStart hbox btnAlign PackNatural 0

    -- Destination combo
    dst <- listStoreNew []
    dstCmb <- comboBoxNewWithModel dst
    hboxDst <- hBoxNew False 5
    boxPackStart hboxDst dstCmb PackNatural 0

    renderer <- cellRendererTextNew
    cellLayoutPackStart dstCmb renderer False
    cellLayoutSetAttributes dstCmb renderer dst (\ind -> [cellText := ind ^. host])

    vbox <- vBoxNew False 5
    boxPackStart vbox hboxDst PackNatural 0
    boxPackStart vbox hbox PackGrow 0

    return (PostBox textModel textView button dst dstCmb, vbox)

setupInstPane =
  do
    -- Tree view
    treeModel <- treeStoreNew []

    treeView <- treeViewNewWithModel treeModel

    col <- treeViewColumnNew
    treeViewColumnSetTitle col ("Instances" :: Text.Text)
    renderer <- cellRendererTextNew
    cellLayoutPackStart col renderer False
    cellLayoutSetAttributes col renderer treeModel (\ind -> [cellText := dsiLabel ind])

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
    set webSettings
      [
        webSettingsEnableScripts := False,
        webSettingsEnablePageCache := True
      ]
    webViewSetWebSettings webView webSettings

    scrollWin <- scrolledWindowNew Nothing Nothing
    scrollWin `containerAdd` webView

    webViewLoadString
        webView
        html
        (Just "text/html")
        "about:armageddon"

    return (StatusPane webView, scrollWin)

--
-- Getter & Modification
--

addRegistration :: T -> Registration -> IO ()
addRegistration this reg =
  do
    bNeedSelect <- isNothing <$> treeStoreLookup (this ^. instModel) [0]

    -- Update status pane
    treeStoreInsertTree (this ^. instModel) [] 0 (makeDsiTree reg)

    if bNeedSelect
      then do
        postGUIAsync $ treeSelectionSelectPath (this ^. instSel) [0]
      else
        return ()

    -- Update toot pane
    listStoreAppend (this ^. postDst) reg
    return ()

getDataSource :: HasInstPane self => self -> IO (Maybe DataSource)
getDataSource tp = runMaybeT $
  do
    rows <- lift $ treeSelectionGetSelectedRows (tp ^. instSel)
    path <- foldr (\x _ -> return x) mzero rows
    dsi <- lift $ treeStoreGetValue (tp ^. instModel) path

    return $ getDs dsi
