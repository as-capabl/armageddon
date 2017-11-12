{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{- LANGUAGE TemplateHaskell -}
{-# LANGUAGE Strict, StrictData #-}

module
    MainForm
where

import qualified GHC.Base

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

-- makeClassy ''PostBox
class HasPostBox c_aCjN where
  postBox :: Lens' c_aCjN PostBox
  postButton :: Lens' c_aCjN Button
  {-# INLINE postButton #-}
  postDst :: Lens' c_aCjN (ListStore Registration)
  {-# INLINE postDst #-}
  postDstCombo :: Lens' c_aCjN ComboBox
  {-# INLINE postDstCombo #-}
  postText :: Lens' c_aCjN TextBuffer
  {-# INLINE postText #-}
  postTextView :: Lens' c_aCjN TextView
  {-# INLINE postTextView #-}
  postButton = (GHC.Base..) postBox postButton
  postDst = (GHC.Base..) postBox postDst
  postDstCombo = (GHC.Base..) postBox postDstCombo
  postText = (GHC.Base..) postBox postText
  postTextView = (GHC.Base..) postBox postTextView
instance HasPostBox PostBox where
  {-# INLINE postButton #-}
  {-# INLINE postDst #-}
  {-# INLINE postDstCombo #-}
  {-# INLINE postText #-}
  {-# INLINE postTextView #-}
  postBox = GHC.Base.id
  postButton f_aCjO (PostBox x1_aCjP x2_aCjQ x3_aCjR x4_aCjS x5_aCjT)
    = fmap
        (\ y1_aCjU -> PostBox x1_aCjP x2_aCjQ y1_aCjU x4_aCjS x5_aCjT)
        (f_aCjO x3_aCjR)
  postDst f_aCjV (PostBox x1_aCjW x2_aCjX x3_aCjY x4_aCjZ x5_aCk0)
    = fmap
        (\ y1_aCk1 -> PostBox x1_aCjW x2_aCjX x3_aCjY y1_aCk1 x5_aCk0)
        (f_aCjV x4_aCjZ)
  postDstCombo
    f_aCk2
    (PostBox x1_aCk3 x2_aCk4 x3_aCk5 x4_aCk6 x5_aCk7)
    = fmap
        (\ y1_aCk8 -> PostBox x1_aCk3 x2_aCk4 x3_aCk5 x4_aCk6 y1_aCk8)
        (f_aCk2 x5_aCk7)
  postText f_aCk9 (PostBox x1_aCka x2_aCkb x3_aCkc x4_aCkd x5_aCke)
    = fmap
        (\ y1_aCkf -> PostBox y1_aCkf x2_aCkb x3_aCkc x4_aCkd x5_aCke)
        (f_aCk9 x1_aCka)
  postTextView
    f_aCkg
    (PostBox x1_aCkh x2_aCki x3_aCkj x4_aCkk x5_aCkl)
    = fmap
        (\ y1_aCkm -> PostBox x1_aCkh y1_aCkm x3_aCkj x4_aCkk x5_aCkl)
        (f_aCkg x2_aCki)
-- end TH

data InstPane = InstPane {
    _instModel :: TreeStore DSItem,
    _instView :: TreeView,
    _instSel :: TreeSelection,
    _instAddBtn :: Button,
    _instMenuBtn :: Button
  }

-- makeClassy ''InstPane
class HasInstPane c_aCp1 where
  instPane :: Lens' c_aCp1 InstPane
  instAddBtn :: Lens' c_aCp1 Button
  {-# INLINE instAddBtn #-}
  instMenuBtn :: Lens' c_aCp1 Button
  {-# INLINE instMenuBtn #-}
  instModel :: Lens' c_aCp1 (TreeStore DSItem)
  {-# INLINE instModel #-}
  instSel :: Lens' c_aCp1 TreeSelection
  {-# INLINE instSel #-}
  instView :: Lens' c_aCp1 TreeView
  {-# INLINE instView #-}
  instAddBtn = (GHC.Base..) instPane instAddBtn
  instMenuBtn = (GHC.Base..) instPane instMenuBtn
  instModel = (GHC.Base..) instPane instModel
  instSel = (GHC.Base..) instPane instSel
  instView = (GHC.Base..) instPane instView
instance HasInstPane InstPane where
  {-# INLINE instAddBtn #-}
  {-# INLINE instMenuBtn #-}
  {-# INLINE instModel #-}
  {-# INLINE instSel #-}
  {-# INLINE instView #-}
  instPane = GHC.Base.id
  instAddBtn
    f_aCp2
    (InstPane x1_aCp3 x2_aCp4 x3_aCp5 x4_aCp6 x5_aCp7)
    = fmap
        (\ y1_aCp8 -> InstPane x1_aCp3 x2_aCp4 x3_aCp5 y1_aCp8 x5_aCp7)
        (f_aCp2 x4_aCp6)
  instMenuBtn
    f_aCp9
    (InstPane x1_aCpa x2_aCpb x3_aCpc x4_aCpd x5_aCpe)
    = fmap
        (\ y1_aCpf -> InstPane x1_aCpa x2_aCpb x3_aCpc x4_aCpd y1_aCpf)
        (f_aCp9 x5_aCpe)
  instModel f_aCpg (InstPane x1_aCph x2_aCpi x3_aCpj x4_aCpk x5_aCpl)
    = fmap
        (\ y1_aCpm -> InstPane y1_aCpm x2_aCpi x3_aCpj x4_aCpk x5_aCpl)
        (f_aCpg x1_aCph)
  instSel f_aCpn (InstPane x1_aCpo x2_aCpp x3_aCpq x4_aCpr x5_aCps)
    = fmap
        (\ y1_aCpt -> InstPane x1_aCpo x2_aCpp y1_aCpt x4_aCpr x5_aCps)
        (f_aCpn x3_aCpq)
  instView f_aCpu (InstPane x1_aCpv x2_aCpw x3_aCpx x4_aCpy x5_aCpz)
    = fmap
        (\ y1_aCpA -> InstPane x1_aCpv y1_aCpA x3_aCpx x4_aCpy x5_aCpz)
        (f_aCpu x2_aCpw)
-- end th

data StatusPane = StatusPane {
    _statusView :: WebView
  }

-- makeClassy ''StatusPane
class HasStatusPane c_aCtJ where
  statusPane :: Lens' c_aCtJ StatusPane
  statusView :: Lens' c_aCtJ WebView
  {-# INLINE statusView #-}
  statusView = (GHC.Base..) statusPane statusView
instance HasStatusPane StatusPane where
  {-# INLINE statusView #-}
  statusPane = GHC.Base.id
  statusView = iso (\ (StatusPane x_aCtK) -> x_aCtK) StatusPane
-- end th

data T = T {
    _win :: Window,
    _pb :: PostBox,
    _ip :: InstPane,
    _sp :: StatusPane
  }

-- makeLenses ''T
ip :: Lens' T InstPane
ip f_aCAF (T x1_aCAG x2_aCAH x3_aCAI x4_aCAJ)
  = fmap
      (\ y1_aCAK -> T x1_aCAG x2_aCAH y1_aCAK x4_aCAJ) (f_aCAF x3_aCAI)
{-# INLINE ip #-}
pb :: Lens' T PostBox
pb f_aCAL (T x1_aCAM x2_aCAN x3_aCAO x4_aCAP)
  = fmap
      (\ y1_aCAQ -> T x1_aCAM y1_aCAQ x3_aCAO x4_aCAP) (f_aCAL x2_aCAN)
{-# INLINE pb #-}
sp :: Lens' T StatusPane
sp f_aCAR (T x1_aCAS x2_aCAT x3_aCAU x4_aCAV)
  = fmap
      (\ y1_aCAW -> T x1_aCAS x2_aCAT x3_aCAU y1_aCAW) (f_aCAR x4_aCAV)
{-# INLINE sp #-}
win :: Lens' T Window
win f_aCAX (T x1_aCAY x2_aCAZ x3_aCB0 x4_aCB1)
  = fmap
      (\ y1_aCB2 -> T y1_aCB2 x2_aCAZ x3_aCB0 x4_aCB1) (f_aCAX x1_aCAY)
{-# INLINE win #-}
--end th

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
