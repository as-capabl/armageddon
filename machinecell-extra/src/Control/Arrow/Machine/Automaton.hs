{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module
    Control.Arrow.Machine.Automaton (
        Automaton(..),
        constructAuto
      )
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Arrow.Machine.Types
import Control.Arrow.Machine.ArrowUtil
import qualified Control.Arrow.Machine.Utils as Mc

class
    Automaton a m p q | a -> m, a -> p, a -> q
  where
    auto :: a -> PlanT p q m r

constructAuto ::
    (Automaton a m i o, Monad m) =>
    a ->
    ProcessT m (Event i) (Event o)
constructAuto = constructT . auto

