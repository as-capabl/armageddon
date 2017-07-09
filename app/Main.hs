{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module
    Main
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Void
import Data.Tree
import Control.Monad (forever)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import qualified Data.Text as Text
import Control.Concurrent (forkIO, threadDelay)

import Graphics.UI.Gtk hiding (on)
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

import qualified BasicModel
import qualified Content
import qualified Async
import qualified MainForm
import qualified AuthDialog

import Debug.Trace

type TheWorld = World IO IO IORefRunner


main :: IO ()
main = gtkReactimate mainArrow


mainArrow :: ProcessT IO TheWorld (Event Void)
mainArrow = proc world ->
  do
    mf <- constructT setup <<< onActivation -< world
    rSwitch muted -< (world, runMainForm <$> mf)
  where
    setup =
      do
        mcli <- lift auth
        frm <- lift $ MainForm.setup Content.initialHtml
        maybe (return ()) (\cli -> yield (frm, cli)) mcli
    auth =
      do
        AuthDialog.showDialog
            "pawoo.net"
            "8013afc7a192c032c6b68dd965116e27a0d614e44c8c252707f23b2596ce8808"
            "3553df5b2d86e69aab6c047c3df60ab853333968a73f1de1ac949460d2946505"

runMainForm :: (MainForm.MainForm, Hdon.HastodonClient) -> ProcessT IO TheWorld (Event Void)
runMainForm (mf, cli) = proc world ->
  do
    treeSelected <- (MainForm.trSel $ MainForm.mfTreePane mf) `on`
        treeSelectionSelectionChanged
            -< world
    dsByTree <- fork <<< fire0 (MainForm.getDataSource $ MainForm.mfTreePane mf) -< treeSelected

    frameLoad <-
        MainForm.mfTootPane mf `on` documentLoadFinished
            -< world

    fetchDs <- gather -< [initDs <$  frameLoad, dsByTree]
    wrSwitch (pure ()) -< ((world, ()), fetch <$> fetchDs)

    tootMouse <-
        MainForm.postButton (MainForm.mfPostBox mf)
            `on` buttonPressEvent
            `looking` ((,) <$> eventClick <*> eventButton)
                -< world
    tootClick <- filterEvent (== (SingleClick, LeftButton)) -< tootMouse
    wrSwitch (pure ()) -< ((world, ()), toot <$ tootClick)

    del <-
        MainForm.mfWindow mf `on` deleteEvent `Replying` False
            -< world
    construct (await >> stop) -< del
  where
    initDs = BasicModel.DataSource "" "" BasicModel.DSHome
    fetch = fetchPublicTimeline (MainForm.mfTootPane mf) cli
    toot = proc (world, _) ->
      do
        fire0 (postToot (MainForm.mfPostBox mf) cli) <<< onActivation -< world
        returnA -< ()

fetchPublicTimeline ::
    WebView ->
    Hdon.HastodonClient ->
    BasicModel.DataSource ->
    ProcessT IO (TheWorld, ()) ()
fetchPublicTimeline wv cli ds0 = proc (world, ()) ->
  do
    fire0 (clearWebView wv) <<< onActivation -< world
    sts <- Async.runResource priorityDefaultIdle fetchThread -< world
    fire (prependStatus wv) -< sts
    returnA -< ()
  where
    fetchThread = constructT $
      do
        Right sts <- liftIO $ initialReadDs ds0
        mapM_ yield $ reverse sts
        auto $ sourceReadDs ds0 C.=$= filterLeftC C.=$= filterUpdateC

    initialReadDs (BasicModel.DataSource _ _ BasicModel.DSHome) = Hdon.getHomeTimeline cli
    initialReadDs _ = Hdon.getPublicTimeline cli

    sourceReadDs (BasicModel.DataSource _ _ BasicModel.DSHome) = Hdon.sourceUserTimeline cli
    sourceReadDs _ = Hdon.sourcePublicTimeline cli

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
        body <- MaybeT $ DOM.getBody doc

        forever $
          do
            ch <- MaybeT $ DOM.getFirstChild body
            DOM.removeChild body (Just ch)


prependStatus wv st = runMaybeT go >> return ()
  where
    go =
      do
        doc <- MaybeT $ webViewGetDomDocument wv
        body <- MaybeT $ DOM.getBody doc

        ch <- MaybeT $ Content.domifyStatus doc st

        mfc <- lift $ DOM.getFirstChild body
        DOM.insertBefore body (Just ch) mfc

postToot form cli =
  do
    let postText = MainForm.postText form
    (beginPos, endPos) <- textBufferGetBounds postText
    content <- textBufferGetText postText beginPos endPos False
    textBufferDelete postText beginPos endPos

    putStrLn content
    forkIO $
      do
        r <- Hdon.postStatus content cli
        print r
        return ()


