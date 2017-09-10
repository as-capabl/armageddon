{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module
    Control.Arrow.Machine.World
      (
        WorldRunner(..),
        World(..),

        listen,
        start,

        onActivation,
        onInactivation,

        HasWorld(),

        wkSwitch,
        wSwitch,
        wrSwitch,
        wrSwitch0,
        wkSwitchAfter,
        wSwitchAfter,
        wFinishWith,

        wConst,
        wHold,
        wAccum,

        Mailbox (),
        mailboxNew,
        mailboxPost,
        onMailboxPost
      )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad (forever, forM_)
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
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


type RootCount = Int

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
    Ref t a ->
    a ->
    m ()
refSetA vWr ref x =
    liftBase $ refSet vWr ref x

-- Event ID
newtype EventID = EventID Int deriving (Eq, Show)

actID = EventID 0
inactID = EventID 1
inclID (EventID n) = EventID (n+1)

newID ::
    (WorldRunner instr m (wr instr m), Monad instr, Monad m) =>
    EventEnv wr m instr -> m EventID
newID env =
  do
    let wr = envGetRun env
        ref = envGetIDPool env
    x <- refGetA wr ref
    refSetA wr ref $ inclID x
    return x

-- Internal data.
data Any
nothingAny :: Any
nothingAny = unsafeCoerce ()

type MainStateProc instr m = (EventID, Any) -> instr (StM m ())
type Posted = [(EventID, Any)]
type MainState instr m = Either Posted (Posted, MainStateProc instr m)

data EventEnv wr m instr =
    EventEnv {
        envGetRootCount :: Ref (wr instr m) RootCount,
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

{-# INLINE listen #-}
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

    myID <- lift $ newID env
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
handleProc env@(EventEnv _ _ vSt) eid arg =
  do
    eSt <- refGet (envGetRun env) vSt
    case eSt
      of
        Left l -> error "World.hs: handler is busy"
        Right (l, st) ->
          do
            refSet (envGetRun env) vSt (Left l)
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
{-# INLINE onActivation #-}

-- |Fires once on initialization.
onInactivation ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    P.ProcessT m (World instr m wr) (P.Event ())
onInactivation = proc world ->
  do
    ea <- listenID -< (world, inactID)
    returnA -< () <$ ea
{-# INLINE onInactivation #-}


--
-- Run sf
--
start ::
    forall instr m wr.
    (WorldRunner instr m (wr instr m), Monad m, Monad instr) =>
    m () ->
    P.ProcessT m (World instr m wr) (P.Event Void) ->
    m ()
start fin sf0 =
  do
    let px = Proxy :: Proxy (wr instr m)
    vRt <- newRefA px 0
    startWithRoot vRt fin sf0

startWithRoot ::
    forall instr m wr.
    (WorldRunner instr m (wr instr m), Monad m, Monad instr) =>
    Ref (wr instr m) RootCount ->
    m () ->
    P.ProcessT m (World instr m wr) (P.Event Void) ->
    m ()
startWithRoot vRt fin sf0 =
  do
    initSt <- embed (\_ -> return ())

    let px = Proxy :: Proxy (wr instr m)
    vID <- newRefA px (inclID inactID)
    vSt <- newRefA px (Left [])

    let env = EventEnv {
            envGetRootCount = vRt,
            envGetIDPool = vID,
            envGetState = vSt
          }

    recurseOnEnv fin env (proc etp -> sf0 -< World env etp) (actID, nothingAny)

modifyRef'A ::
    (WorldRunner instr m t, Monad instr, Monad m) =>
    proxy t ->
    Ref t a ->
    (a -> (a, b)) ->
    m b
modifyRef'A rr ref f =
  do
    x <- refGetA rr ref
    let (!y, r) = f x
    refSetA rr ref y
    return r

recurseOnEnv ::
    (Monad m, MonadBaseControl instr m, Monad instr, WorldRunner instr m (wr instr m)) =>
    m () ->
    EventEnv wr m instr ->
    P.ProcessT m (P.Event (EventID, Any)) (P.Event c) ->
    (EventID, Any) ->
    m ()
recurseOnEnv fin env p0 etp0 =
    runMaybeT go >>= maybe runFin return
  where
    go =
      do
        p' <- runBody p0 [etp0]
        st <- lift $ embed (recurseOnEnv fin env p')
        lift $ refSetA refrun (envGetState env) $ Right ([], st)
    runBody p1 etps =
      do
        p' <- execStateT `flip` p1 $ forM_ etps $ \etp ->
          do
            p <- get
            p' <-
                P.stepRun
                    (lift . lift)
                    (\_ -> return ())
                    (\_ -> lift $ MaybeT $ return Nothing)
                    p etp
            put p'
        next <- lift $ modifyRef'A refrun (envGetState env) $ \case
            Left [] -> (Left [], return p')
            Left l -> (Left [], runBody p' $ reverse l)
            Right _ -> error "Internal error at World.hs l302"
        next
    runFin =
      do
        cnt' <- modifyRef'A refrun (envGetRootCount env) (\cnt -> (cnt-1, cnt-1))
        if cnt' <= 0 then fin else return ()
    refrun = envGetRun env

-- |Actuate brand-new handler at its own initialization.
{-
forkWorld ::
    (Monad m, MonadBaseControl instr m, Monad instr, WorldRunner instr m (wr instr m)) =>
    P.ProcessT m (World instr m wr) (P.Event Void) ->
    P.ProcessT m (World instr m wr) ()
forkWorld pa = P.evolve go >>> pure ()
  where
    go =
      do
        env <- wSwitchAfter $ proc world ->
          do
            act <- onActivation -< world
            mut <- P.muted -< world
            returnA -< (mut :: P.Event Void, worldGetEnv world <$ act)
        lift $ startWithRoot (envGetRootCount env) pa
        P.stop
-}

--
-- World switch
--
class
    HasWorld instr m wr a | a -> instr, a -> m, a -> wr
  where
    getWorld :: a -> World instr m wr
    modWorld :: (World instr m wr -> World instr m wr) -> a -> a

instance
    HasWorld instr m wr (World instr m wr)
  where
    getWorld = id
    modWorld = id

instance
    HasWorld instr m wr (World instr m wr, a)
  where
    getWorld = fst
    modWorld = first

prefeeder ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    EventID ->
    P.ProcessT m w (w, P.Event ())
prefeeder eid = proc world ->
  do
    n <- P.now -< ()
    tp <- P.fork -< [Left (eid, nothingAny), Right ()] <$ n

    let input = modWorld (\w -> w {worldGetEvent = P.filterLeft tp}) world
        trigger = P.filterRight tp

    returnA -< (input, trigger)

{-# INLINE wkSwitch #-}
wkSwitch ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    P.ProcessT m w c ->
    P.ProcessT m (w, c) (P.Event t) ->
    (P.ProcessT m w c -> t -> P.ProcessT m w c) ->
    P.ProcessT m w c
wkSwitch sf test k = P.evolve $
  do
    (sf', t) <- P.dkSwitchAfter test sf
    (sf'', _) <- P.gSwitchAfter (prefeeder inactID) id sf'
    (sf''', _) <- P.gSwitchAfter (prefeeder actID) id $ k sf'' t
    P.finishWith sf'''

{-# INLINE wSwitch #-}
wSwitch ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    P.ProcessT m w (c, P.Event t) ->
    (t -> P.ProcessT m w c) ->
    P.ProcessT m w c
wSwitch sf k = P.evolve $
  do
    (sf', t) <- P.dgSwitchAfter (id &&& pure ()) (arr fst) sf
    _ <- P.gSwitchAfter (prefeeder inactID) (first (arr fst)) sf'
    (sf''', _) <- P.gSwitchAfter (prefeeder actID) id $ k t
    P.finishWith sf'''


{-# INLINE wrSwitch #-}
wrSwitch ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    P.ProcessT m w c ->
    P.ProcessT m (w, P.Event (P.ProcessT m w c)) c
wrSwitch sf = P.evolve $
  do
    (sf', nx) <- P.dgSwitchAfter id id sf
    _ <- P.gSwitchAfter (arr fst >>> prefeeder inactID) id sf'
    (nx', _) <- P.gSwitchAfter (arr fst >>> prefeeder actID) id nx
    P.finishWith $ wrSwitch nx'

{-# INLINE wrSwitch0 #-}
wrSwitch0 ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m, P.Occasional c) =>
    P.ProcessT m (w, P.Event (P.ProcessT m w c)) c
wrSwitch0 = wrSwitch (P.muted . arr getWorld)

{-# INLINE wSwitchAfter #-}
wSwitchAfter ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    P.ProcessT m w (c, P.Event r) ->
    P.Evolution w c m r
wSwitchAfter sf = P.Evolution $ cont $ wSwitch sf

{-# INLINE wkSwitchAfter #-}
wkSwitchAfter ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    P.ProcessT m (w, c) (P.Event r) ->
    P.ProcessT m w c ->
    P.Evolution w c m (P.ProcessT m w c, r)
wkSwitchAfter test sf = P.Evolution $ cont $ wkSwitch sf test . curry

{-# INLINE wFinishWith #-}
wFinishWith ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr w, Monad m) =>
    P.ProcessT m w c ->
    P.Evolution w c m r
wFinishWith sf = P.Evolution $ cont $ \_ -> P.evolve $
  do
    (sf', ()) <- P.gSwitchAfter (prefeeder actID) id sf
    P.finishWith sf'

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

--
-- Message box
--
data Mailbox (instr :: * -> *) (m :: * -> *) wr a = Mailbox
  {
    mbGetID :: EventID,
    mbGetRef :: Ref (wr instr m) (MainState instr m)
  }


{-# INLINE mailboxNew #-}
mailboxNew ::
    (WorldRunner instr m (wr instr m), HasWorld instr m wr i, Monad m, P.Occasional o) =>
    P.Evolution i o m (Mailbox instr m wr a)
mailboxNew =
  do
    env <- wSwitchAfter $ proc x ->
      do
        let world = getWorld x
        act <- onActivation -< world
        mut <- P.muted -< world
        returnA -< (mut, worldGetEnv world <$ act)
    evtId <- lift $ newID env
    return $ Mailbox evtId (envGetState env)

{-# INLINE mailboxPost #-}
mailboxPost ::
    forall instr m wr a.
    (WorldRunner instr m (wr instr m), Monad m) =>
    Mailbox instr m wr a -> a -> instr ()
mailboxPost mb x =
  do
    let rr = Proxy :: Proxy (wr instr m)
        etp = (mbGetID mb, unsafeCoerce x)
        vSt = mbGetRef mb
    eSt <- refGet rr vSt
    case eSt
      of
        Left l -> refSet rr vSt $ Left (etp : l)
        Right (l, st) ->
          do
            refSet rr vSt $ Left l
            st etp
            return ()

{-# INLINE onMailboxPost #-}
onMailboxPost ::
    (WorldRunner instr m (wr instr m), Monad m) =>
    Mailbox instr m wr a ->
    P.ProcessT m (World instr m wr) (P.Event a)
onMailboxPost mb = proc world ->
  do
    P.evMap unsafeCoerce <<< listenID -< (world, mbGetID mb)
