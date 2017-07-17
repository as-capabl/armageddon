{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleContexts #-}

module
    Control.Arrow.Machine.World
      (
        WorldRunner(..),
        World(..),

        listen,
        start,

        onActivation,
        onInactivation,

        wkSwitch,
        wSwitch,
        wrSwitch,
        wrSwitch0,
        wkSwitchAfter,
        wSwitchAfter,

        wConst,
        wHold,
        wAccum
      )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad (forever)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Base
import Control.Monad.Cont (cont, runCont)
import qualified Control.Arrow.Machine as P
import qualified Control.Arrow.Machine.Misc.Discrete as D
import Unsafe.Coerce
import Data.Void
import Data.Proxy
import Data.Monoid (Last(..))
import Data.Maybe (catMaybes)
import Control.Monad.Fix



class
    MonadBaseControl instr m =>
    WorldRunner instr m t | t -> m, t -> instr
  where
    type Ref t :: * -> *

    newRef :: proxy t -> a -> instr (Ref t a)
    refGet :: proxy t -> Ref t a -> instr a
    refSet :: proxy t -> Ref t a -> a -> instr ()

newRefA ::
    (WorldRunner instr m t, Monad instr, Monad m) =>
    proxy t ->
    a ->
    m (Ref t a)
newRefA vWr x =
    liftBase $ newRef vWr x

refGetA ::
    (WorldRunner instr m t, Monad instr, Monad m) =>
    proxy t ->
    Ref t a ->
    m a
refGetA vWr ref =
    liftBase $ refGet vWr ref

refSetA ::
    (WorldRunner instr m t, Monad instr, Monad m) =>
    proxy t ->
    (Ref t a, a) ->
    m ()
refSetA vWr (ref, x) =
    liftBase $ refSet vWr ref x

-- Event ID
newtype EventID = EventID Int deriving (Eq, Show)

actID = EventID 0
inactID = EventID 1
inclID (EventID n) = EventID (n+1)
newID wr env =
  do
    let ref = envGetIDPool env
    x <- refGetA wr ref
    refSetA wr (ref, inclID x)
    return x

-- Internal data.
data Any
nothingAny :: Any
nothingAny = unsafeCoerce ()

type MainState instr m = (EventID, Any) -> instr (StM m ())

data EventEnv wr m instr =
    EventEnv {
        envGetIDPool :: Ref (wr instr m) EventID,
        envGetState :: Ref (wr instr m) (MainState instr m)
      }

envGetRun :: EventEnv wr m instr -> Proxy (wr instr m)
envGetRun _ = Proxy


data World instr m wr =
    World {
        worldGetEnv :: EventEnv wr m instr,
        worldGetEvent :: P.Event (EventID, Any)
      }

instance
    P.Occasional' (World instr m wr)
  where
    collapse = P.collapse . worldGetEvent


-- Internal
listenID ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr, EventID) (P.Event Any)
listenID = proc (World _ etp, myID) ->
  do
    returnA -< P.filterJust $ go myID <$> etp
  where
    go myID (curID, ea) | curID == myID = Just ea
    go _ _ = Nothing

-- Event listening
listen ::
    (WorldRunner instr m (wr instr m), Monad m, Monad instr) =>
    ((a -> instr ()) -> m h) ->
    (h -> m ()) ->
    P.ProcessT m (World instr m wr) (P.Event a)
listen reg disposer = P.evolve $ forever $
  do
    env <- P.switchAfter $ proc (world@(World env _)) ->
      do
        act <- onActivation -< world
        P.muted *** returnA -< (world, env <$ act)

    myID <- lift $ newID (envGetRun env) env
    h <- lift $ reg (handleProc env myID . unsafeCoerce)

    P.switchAfter $ proc world ->
      do
        inact <- onInactivation -< world
        ea <- listenID -< (world, myID)
        returnA -< (unsafeCoerce <$> ea, inact)

    lift $ disposer h


handleProc ::
    (WorldRunner instr m (wr instr m), Monad instr, Monad m) =>
    EventEnv wr m instr ->
    EventID ->
    Any ->
    instr ()
handleProc env@(EventEnv _ vSt) eid arg =
  do
    st <- refGet (envGetRun env) vSt
    st (eid, arg)
    return ()


-- |Fires once on initialization.
onActivation ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr) (P.Event ())
onActivation = proc world ->
  do
    ea <- listenID -< (world, actID)
    returnA -< () <$ ea

-- |Fires once on initialization.
onInactivation ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr) (P.Event ())
onInactivation = proc world ->
  do
    ea <- listenID -< (world, inactID)
    returnA -< () <$ ea

--
-- Run sf
--
start ::
    forall instr m wr.
    m () ->
    (WorldRunner instr m (wr instr m), Monad m, Monad instr) =>
    P.ProcessT m (World instr m wr) (P.Event Void) ->
    m ()
start fin sf0 =
  do
    initSt <- embed (\_ -> return ())

    vID <- newRefA (Proxy :: Proxy (wr instr m)) (inclID inactID)
    vSt <- newRefA (Proxy :: Proxy (wr instr m)) initSt

    let env = EventEnv {
            envGetIDPool = vID,
            envGetState = vSt
          }

    recurseOnEnv fin env [proc etp -> sf0 -< World env etp] (actID, nothingAny)

recurseOnEnv ::
    (Monad m, MonadBaseControl instr m, Monad instr, WorldRunner instr m (wr instr m)) =>
    m () ->
    EventEnv wr m instr ->
    [P.ProcessT m (P.Event (EventID, Any)) (P.Event c)] ->
    (EventID, Any) ->
    m ()
recurseOnEnv fin env ps etp =
  do
    mps' <- traverse runIt ps
    let ps' = catMaybes mps'
    if null ps'
      then
        fin
      else do
        st <- embed (recurseOnEnv fin env ps')
        refSetA (envGetRun env) (envGetState env, st)
  where
    runIt p = runMaybeT $ P.stepRun lift (\_ -> return ()) (\_ -> MaybeT $ return Nothing) p etp

--
-- World switch
--
prefeeder ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    EventID ->
    P.ProcessT m (World instr m wr, b) ((World instr m wr, b), P.Event ())
prefeeder eid = proc (world@(World env etp), x) ->
  do
    n <- P.now -< ()
    tp <- P.fork -< [Left (eid, nothingAny), Right ()] <$ n

    let input = (world {worldGetEvent = P.filterLeft tp}, x)
        trigger = P.filterRight tp

    returnA -< (input, trigger)

wkSwitch ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr, b) c ->
    P.ProcessT m ((World instr m wr, b), c) (P.Event t) ->
    (P.ProcessT m (World instr m wr, b) c -> t -> P.ProcessT m (World instr m wr, b) c) ->
    P.ProcessT m (World instr m wr, b) c
wkSwitch sf test k = P.evolve $
  do
    (sf', t) <- P.dkSwitchAfter test sf
    (sf'', _) <- P.gSwitchAfter (prefeeder inactID) id sf'
    (sf''', _) <- P.gSwitchAfter (prefeeder actID) id $ k sf'' t
    P.finishWith sf'''

wSwitch ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr, b) (c, P.Event t) ->
    (t -> P.ProcessT m (World instr m wr, b) c) ->
    P.ProcessT m (World instr m wr, b) c
wSwitch sf k = P.evolve $
  do
    (sf', t) <- P.dgSwitchAfter (id &&& pure ()) (arr fst) sf
    _ <- P.gSwitchAfter (prefeeder inactID) (first (arr fst)) sf'
    (sf''', _) <- P.gSwitchAfter (prefeeder actID) id $ k t
    P.finishWith sf'''


wrSwitch ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr, b) c ->
    P.ProcessT m ((World instr m wr, b), P.Event (P.ProcessT m (World instr m wr, b) c)) c
wrSwitch sf = P.evolve $
  do
    (sf', nx) <- P.dgSwitchAfter id id sf
    _ <- P.gSwitchAfter (arr fst >>> prefeeder inactID) id sf'
    (nx', _) <- P.gSwitchAfter (arr fst >>> prefeeder actID) id nx
    P.finishWith $ wrSwitch nx'

wrSwitch0 ::
    (WorldRunner instr m (wr instr m), Monad m, P.Occasional c) =>
    P.ProcessT m ((World instr m wr, b), P.Event (P.ProcessT m (World instr m wr, b) c)) c
wrSwitch0 = wrSwitch (P.muted . arr fst)

wSwitchAfter ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr, b) (c, P.Event r) ->
    P.Evolution (World instr m wr, b) c m r
wSwitchAfter sf = P.Evolution $ cont $ wSwitch sf

wkSwitchAfter ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m ((World instr m wr, b), c) (P.Event r) ->
    P.ProcessT m (World instr m wr, b) c ->
    P.Evolution (World instr m wr, b) c m (P.ProcessT m (World instr m wr, b) c, r)
wkSwitchAfter test sf = P.Evolution $ cont $ wkSwitch sf test . curry

--
-- World discrete
--
wConst ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    b ->
    P.ProcessT m (World instr m wr) (D.T b)
wConst x = arr (id &&& pure P.noEvent) >>> wHold x

wHold ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    b ->
    P.ProcessT m (World instr m wr, P.Event b) (D.T b)
wHold x = arr (id *** fmap const) >>> wAccum x

wAccum ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    b ->
    P.ProcessT m (World instr m wr, P.Event (b -> b)) (D.T b)
wAccum x = proc (world, ev) ->
  do
    init <- onActivation -< world
    rec
        r <- P.rSwitch (D.unsafeConstant x) -< ((), merge' (D.value r) init ev)
    returnA -< r
  where
    merge' r init ev =
        let
            initS = fmap (Last . Just) $ D.constant r <$ init
            updateS = fmap (Last . Just) $ fmap D.constant $ fmap ($r) $ ev
          in
            P.filterJust $ fmap getLast $ initS `mappend` updateS

