{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module
    Async
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Void
import Data.Tree
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.Text as Text

import Graphics.UI.Gtk

import Control.Arrow.Machine hiding (run)
import qualified Graphics.UI.McGtk as Mg
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner

import Control.Concurrent (forkIO, threadDelay, killThread)
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as Unagi

type TheWorld = World IO IO IORefRunner

run ::
    Priority ->
    ProcessT IO (Event ()) (Event a) ->
    ProcessT IO TheWorld (Event a)
run = run' id

runResource ::
    Priority ->
    ProcessT (ResourceT IO) (Event ()) (Event a) ->
    ProcessT IO TheWorld (Event a)
runResource = run' runResourceT

run' runner priority body = proc world ->
    wSwitch (muted *** (pure noEvent >>> ignite)) (\arg -> arr fst >>> listenIt arg)
        -< (world, world)
  where
    ignite = constructT $
      do
        (inC, outC) <- lift $ Unagi.newChan
        threadId <- lift $ forkIO $ runner $ runT (liftIO . Unagi.writeChan inC) body (repeat ())
        yield (outC, threadId)

    listenIt (outC, threadId) = proc world ->
      do
        idle <- listen
            (\h -> idleAdd (do {b <- Unagi.isActive outC; h b; return b}) priority)
            (\sigId -> idleRemove sigId >> killThread threadId)
                -< world
        constructT (consume outC) -< idle

    consume outC =
      do
        el <- lift $ Unagi.tryReadChan outC
        consumeEl outC el

    consumeEl outC el =
      do
        mx <- lift $ Unagi.tryRead el
        el' <- case mx
          of
            Just x ->
              do
                yield x
                lift $ Unagi.tryReadChan outC
            Nothing ->
              do
                b <- await
                if not b then stop else return el
        consumeEl outC el'

