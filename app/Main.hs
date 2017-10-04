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
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (forever, guard, forM_, mplus)
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
import qualified Graphics.UI.Gtk.WebKit.DOM.EventM as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Event as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.MouseEvent as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.UIEvent as DOM
import qualified Graphics.UI.Gtk.WebKit.WebView as DOM

import Control.Arrow.Machine
import Graphics.UI.McGtk
import qualified Graphics.UI.McWebkit as McWeb
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

--
-- Utility
--
wrSwitchDiff ::
    Eq a =>
    (a -> ProcessT IO TheWorld b) ->
    a ->
    ProcessT IO (TheWorld, Event a) b
wrSwitchDiff f x0 = evolve $
  do
    x <- wSwitchAfter $ f x0 *** filterEvent (/= x0)
    finishWith $ wrSwitchDiff f x



main :: IO ()
main = gtkReactimate $ evolve $
  do
    model <- DataModel.init

    switchAfter $
        muted &&& onActivation

    mf <- lift $ MainForm.setup Content.initialHtml
    _ <- wSwitchAfter $
        muted &&& (mf ^. MainForm.statusView `on` DOM.loadFinished)

    finishWith $ driveMainForm model mf


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
    ld <-
        onActivation
            -< world
    doc <-
        filterJust
        <<< fire0 (webViewGetDomDocument $ mf ^. MainForm.statusView)
            -< collapse ld
    wrSwitch0 -< (world, driveDocument model <$> doc)

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
    toot = proc world ->
      do
        fire0 (postToot $ mf ^. MainForm.postBox) <<< onActivation -< world

driveDocument ::
    DataModel.T -> DOM.Document -> ProcessT IO TheWorld (Event Void)
driveDocument model doc = proc world ->
  do
    ds <- DataModel.onSelDS model -< world

    wrSwitch0 -< (world, driveDs <$> ds)

    fire (\(placeId, sts, noLeft) -> replaceWithStatus doc sts placeId noLeft)
        <<< DataModel.onUpdateRange model
            -< world

    cl <- McWeb.onSelector doc
        (DOM.EventName "click" :: DOM.EventName DOM.Document DOM.MouseEvent)
        ("div.hdon_username" :: BMText)
        return
            -< world
    fire0 $ putStrLn "Hello!" -< collapse cl
    muted -< world
  where
    driveDs ds = proc world ->
      do
        fire0 (clearWebView doc) <<< onActivation -< world

        scr <- fire0 (isScrollTop doc) <<<
            McWeb.on doc
                (DOM.EventName "scroll" :: DOM.EventName DOM.Document DOM.UIEvent)
                (return ())
                    -< world
        wrSwitchDiff (driveDsSub ds) True -< (world, scr)

    driveDsSub ds True = proc world ->
      do
        fire0 (setRPH ds) <<< onActivation -< world
        fetchPublicTimeline doc ds -< world

    driveDsSub ds False = proc world ->
      do
        muted -< world

    setRPH ds = runMaybeT $
      do
        rphId <- MaybeT $ Content.pushRPH doc
        rph <- MaybeT $ Content.extractRPH rphId doc
        liftIO $ DataModel.requireRange model ds rph

isScrollTop :: DOM.Document -> IO Bool
isScrollTop doc = fmap (fromMaybe True) $ runMaybeT $
  do
    body <- MaybeT $ DOM.getBody doc
    pos <- DOM.getScrollTop body
    return $ pos == 0

fetchPublicTimeline ::
    DOM.Document ->
    BasicModel.DataSource ->
    ProcessT IO TheWorld (Event Void)
fetchPublicTimeline doc ds0 = proc world ->
  do
    sts <- Async.runResource
        (Async.PollTimeout 100000 priorityDefaultIdle)
        (DataModel.readDS ds0)
            -< world
    muted <<< fire (prependStatus doc) -< sts

clearWebView doc = runMaybeT go >> return ()
  where
    go =
      do
        body <- MaybeT $ Content.getTimelineParent doc

        forever $ -- Until getFirstChild fails
          do
            ch <- MaybeT $ DOM.getFirstChild body
            DOM.removeChild body (Just ch)

checkExist :: DOM.Document -> BMText -> MaybeT IO ()
checkExist doc tId =
  do
    pre <- DOM.getElementById doc tId
    guard $ isNothing pre

prependStatus doc st = runMaybeT go >> return ()
  where
    go =
      do
        body <- MaybeT $ Content.getTimelineParent doc
        checkExist doc (statusIdToDomId $ Hdon.statusId st)

        -- Prepend
        ch <- MaybeT $ Content.domifyStatus doc st

        mfc <- lift $ DOM.getFirstChild body
        DOM.insertBefore body (Just ch) mfc

replaceWithStatus doc sts placeId del = runMaybeT go >> return ()
  where
    go =
      do
        body <- MaybeT $ Content.getTimelineParent doc
        frag <- MaybeT $ DOM.createDocumentFragment doc

        forM_ sts $ \st ->
          do
            checkExist doc (statusIdToDomId $ Hdon.statusId st)

            -- Prepend
            ch <- MaybeT $ Content.domifyStatus doc st
            DOM.appendChild frag (Just ch)
            return ()
          `mplus` return ()

        placeElem <- DOM.getElementById doc placeId
        DOM.insertBefore body (Just frag) placeElem

        if del then DOM.removeChild body placeElem >> return () else return ()

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


