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
mainArrow = evolve $
  do
    switchAfter $
        muted &&& onActivation
    mfcli <- switchAfter $ proc world ->
        muted &&& constructT setup -< collapse world
    finishWith $
        runMainForm mfcli
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

runMainForm :: (MainForm.T, Hdon.HastodonClient) -> ProcessT IO TheWorld (Event Void)
runMainForm (mf, cli) = proc world ->
  do
    frameLoad <-
        mf ^. MainForm.statusView `on` documentLoadFinished
            -< world

    treeSelected <-
        mf ^. MainForm.instSel `on` treeSelectionSelectionChanged
            -< world

    newDs <-
        filterJust <<< fire0 (MainForm.getDataSource mf)
            -< treeSelected `mappend` collapse frameLoad

    wrSwitch0 -< ((world, ()), fetch <$> newDs)

    tootClick <-
        onClicked $ mf ^. MainForm.postButton
            -< world
    wrSwitch0 -< ((world, ()), toot <$ tootClick)

    del <-
        mf ^. MainForm.win `on` deleteEvent `Replying` False
            -< world
    construct (await >> stop) -< del
  where
    initDs = BasicModel.DataSource "" "" BasicModel.DSHome
    fetch = fetchPublicTimeline (mf ^. MainForm.statusView) cli
    toot = proc (world, _) ->
      do
        fire0 (postToot (mf ^. MainForm.postBox) cli) <<< onActivation -< world

fetchPublicTimeline ::
    WebView ->
    Hdon.HastodonClient ->
    BasicModel.DataSource ->
    ProcessT IO (TheWorld, ()) (Event Void)
fetchPublicTimeline wv cli ds0 = proc (world, ()) ->
  do
    fire0 (clearWebView wv) <<< onActivation -< world
    sts <- Async.runResource priorityDefaultIdle fetchThread -< world
    muted <<< fire (prependStatus wv) -< sts
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

        forever $ -- Until getFirstChild fails
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
    let postText = form ^. MainForm.postText
    (beginPos, endPos) <- textBufferGetBounds postText
    content <- textBufferGetText postText beginPos endPos False
    textBufferDelete postText beginPos endPos

    putStrLn content
    forkIO $
      do
        r <- Hdon.postStatus content cli
        print r
        return ()


