{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict, StrictData #-}

module
    Main
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Lens hiding (set)
import Data.Void
import Data.Tree
import Control.Monad (forever)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import qualified Data.Text as Text
import Control.Concurrent (forkIO, threadDelay)

import Graphics.UI.Gtk hiding (on, onClicked)
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.ModelView.TreeStore
import Graphics.UI.Gtk.ModelView.TreeView
import qualified Graphics.UI.Gtk.WebKit.DOM.Document as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Node as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration as DOM

import qualified Data.Conduit as C
import qualified Data.Conduit.List as C

import Control.Arrow.Machine
import Graphics.UI.McGtk
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner
import Control.Arrow.Machine.ConduitAdaptor

import qualified Web.Hastodon as Hdon

import BasicModel
import qualified Content
import qualified DataModel
import qualified DB
import qualified Async
import qualified MainForm
import qualified AuthDialog

import Debug.Trace

type TheWorld = World IO IO IORefRunner


main :: IO ()
main = gtkReactimate $ evolve $
  do
    model <- DataModel.init

    switchAfter $
        muted &&& onActivation

    frm <- lift $ MainForm.setup Content.initialHtml
    finishWith $ driveMainForm model frm


authNew :: DataModel.T -> ProcessT IO (TheWorld, Event ()) (Event Void)
authNew model = evolve $ forever $
  do
    wSwitchAfter $ muted *** id
    liftIO $ postGUIAsync $
      do
        mreg <- showDlg
        case mreg
          of
            Just reg -> liftIO $ DataModel.addRegistration model reg
            Nothing -> return ()
  where
    showDlg =
      do
        AuthDialog.authPasswd
            "pawoo.net"
            "8013afc7a192c032c6b68dd965116e27a0d614e44c8c252707f23b2596ce8808"
            "3553df5b2d86e69aab6c047c3df60ab853333968a73f1de1ac949460d2946505"



driveMainForm :: DataModel.T -> MainForm.T -> ProcessT IO TheWorld (Event Void)
driveMainForm model mf = proc world ->
  do
    fire0 (DataModel.loadSetting model) <<< onActivation -< world

    -- Instance Pane
    addClick <-
        onClicked $ mf ^. MainForm.instAddBtn
            -< world

    authNew model -< (world, addClick)

    modelReg <- DataModel.onAddReg model -< world
    fire $ MainForm.addRegistration mf -< modelReg

    treeSelected <-
        mf ^. MainForm.instSel `on` treeSelectionSelectionChanged
            -< world
    newDs <-
        filterJust <<< fire0 (MainForm.getDataSource mf)
            -< treeSelected
    fire $ DataModel.selDS model -< newDs

    -- Status pane
    ds <- DataModel.onSelDS model -< world
    wrSwitch0 -< (world, fetch <$> ds)

    -- Post Box
    tootClick <-
        onClicked $ mf ^. MainForm.postButton
            -< world
    wrSwitch0 -< (world, toot <$ tootClick)

    -- Termination
    del <-
        mf ^. MainForm.win `on` deleteEvent `Replying` False
            -< world
    construct (await >> stop) -< del
  where
    fetch = fetchPublicTimeline (mf ^. MainForm.statusView)
    toot = proc world ->
      do
        fire0 (postToot $ mf ^. MainForm.postBox) <<< onActivation -< world

fetchPublicTimeline ::
    WebView ->
    BasicModel.DataSource ->
    ProcessT IO TheWorld (Event Void)
fetchPublicTimeline wv ds0 = proc world ->
  do
    fire0 (clearWebView wv) <<< onActivation -< world
    sts <- Async.runResource (Async.PollIdle priorityDefaultIdle) fetchThread -< world
    muted <<< fire (prependStatus wv) -< sts
  where
    fetchThread = constructT $
      do
        Right sts <- liftIO $ initialReadDs ds0
        mapM_ yield $ reverse sts
        auto $ sourceReadDs ds0 C.=$= filterLeftC C.=$= filterUpdateC

    initialReadDs ds@(DataSource _ DSHome) = Hdon.getHomeTimeline (ds ^. hastodonClient)
    initialReadDs ds = Hdon.getPublicTimeline (ds ^. hastodonClient)

    sourceReadDs ds@(DataSource cli DSHome) = Hdon.sourceUserTimeline (ds ^. hastodonClient)
    sourceReadDs ds = Hdon.sourcePublicTimeline (ds ^. hastodonClient)

    filterLeftC = C.awaitForever $ \case
        Left err -> trace ("No Parse: " ++ err) $ return ()
        Right x -> C.yield x

    filterUpdateC = C.awaitForever $ \case
        Hdon.StreamUpdate x -> C.yield x
        _ -> return ()


clearWebView wv = runMaybeT go >> return ()
  where
    go =
      do
        doc <- MaybeT $ webViewGetDomDocument wv
        body <- MaybeT $ Content.getTimelineParent doc

        forever $ -- Until getFirstChild fails
          do
            ch <- MaybeT $ DOM.getFirstChild body
            DOM.removeChild body (Just ch)


prependStatus wv st = runMaybeT go >> return ()
  where
    go =
      do
        doc <- MaybeT $ webViewGetDomDocument wv
        body <- MaybeT $ Content.getTimelineParent doc

        ch <- MaybeT $ Content.domifyStatus doc st

        mfc <- lift $ DOM.getFirstChild body
        DOM.insertBefore body (Just ch) mfc

postToot form =
  do
    -- Get text
    let postText = form ^. MainForm.postText
    (beginPos, endPos) <- textBufferGetBounds postText
    content <- textBufferGetText postText beginPos endPos False

    runMaybeT $
      do
        -- Get destination
        postItr <- MaybeT $ comboBoxGetActiveIter $ form ^. MainForm.postDstCombo
        reg <- lift $ listStoreGetValue (form ^. MainForm.postDst) (listStoreIterToIndex postItr)

        -- Post it
        lift $ forkIO $
          do
            r <- Hdon.postStatus content (reg ^. hastodonClient)
            print r
            return ()

        -- Clear text area
        lift $ textBufferDelete postText beginPos endPos

    return ()


