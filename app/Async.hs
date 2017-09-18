{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict, StrictData #-}

module
    Async
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Void
import Data.Tree
import Control.Monad (forever)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource
import qualified Data.Text as Text

import Graphics.UI.Gtk hiding (get)

import Control.Arrow.Machine hiding (run)
import qualified Graphics.UI.McGtk as Mg
import Control.Arrow.Machine.World
import Control.Arrow.Machine.IORefRunner

import Control.Concurrent (forkIO, threadDelay, killThread)
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as Unagi

type TheWorld = World IO IO IORefRunner


data PollStrategy = PollIdle Priority | PollTimeout Priority Int

pollStart (PollIdle priority) f = idleAdd f priority
pollStart (PollTimeout priority time) f = timeoutAddFull f priority time

pollEnd (PollIdle _) = idleRemove
pollEnd (PollTimeout _ _) = timeoutRemove

run ::
    PollStrategy ->
    ProcessT IO (Event ()) (Event a) ->
    ProcessT IO TheWorld (Event a)
run = run' forkIO

runResource ::
    PollStrategy ->
    ProcessT (ResourceT IO) (Event ()) (Event a) ->
    ProcessT IO TheWorld (Event a)
runResource = run' (forkIO . runResourceT)

run' runner poller body = proc world ->
    wSwitch (muted *** (pure noEvent >>> ignite)) (\arg -> arr fst >>> listenIt arg)
        -< (world, world)
  where
    ignite = constructT $
      do
        (inC, outC) <- lift $ Unagi.newChan
        threadId <- lift $ runner $ runT (liftIO . Unagi.writeChan inC) body (repeat ())
        yield (outC, threadId)

    listenIt (outC, threadId) = proc world ->
      do
        idle <- listen
            (\h -> pollStart poller (do {b <- Unagi.isActive outC; h b; return b}))
            (\sigId -> killThread threadId >> pollEnd poller sigId)
                -< world
        constructT $ consume outC -< idle


consume outC =
  do
    el <- lift $ Unagi.tryReadChan outC
    consumeEl outC el
  where
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

{-
consume ::
    MonadIO m =>
    Unagi.OutChan a -> ProcessT m (Event Bool) (Event a)
consume outC = evolve $
  do
    el0 <- liftIO $ Unagi.tryReadChan outC
    evalStateT `flip` el0 $ forever $
      do
        el <- get
        mx <- liftIO $ Unagi.tryRead el
        case mx
          of
            Just x ->
              do
                lift $ yield x
                el' <- liftIO $ Unagi.tryReadChan outC
                put el'
            Nothing ->
              do
                b <- lift await
                lift $ if b then return () else stop
-}



