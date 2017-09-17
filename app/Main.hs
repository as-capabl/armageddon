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

import Control.Arrow.Machine
import Graphics.UI.McGtk
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner

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
    liftIO $ postGUIAsync $ runMaybeT doAuth >> return ()

  where
    doAuth =
      do
        (hostname, appname) <- MaybeT AuthDialog.getHostname

        hst <- MaybeT $ DataModel.getClientInfo hostname appname
        let cid = Text.unpack $ hst ^. clientId
            csecret = Text.unpack $ hst ^. clientSecret

        reg <- MaybeT $ AuthDialog.authPasswd hostname cid csecret
        liftIO $ DataModel.addRegistration model reg


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
    sts <- Async.runResource
        (Async.PollIdle priorityDefaultIdle)
        (DataModel.readDS ds0)
            -< world
    muted <<< fire (prependStatus wv) -< sts

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


