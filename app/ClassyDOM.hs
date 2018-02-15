{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module
    ClassyDOM
where

import GHC.TypeLits
import qualified Data.Tree as Tr
import qualified Data.Text as Tx

-- Types
data TagKind = A | Div | Span

data Attr = Attr
  {
    _id :: Maybe Tx.Text,
    _class :: [Tx.Text]
  }

data NodeV = Node TagKind Attr | TextNode Tx.Text


-- Type kinds
data TreeT = NodeT TagKind Symbol [TreeT] | TextT Symbol

-- Classes
class Template (name :: Symbol)
  where
    type Structure name :: TreeT

class BuildChildren s l
  where {}

instance BuildChildren (TextT n) l
  where {}

instance BuildChildren (NodeT t n '[]) l
  where {}

instance
    (BuildChildren (NodeT t n chs) l, SubStructurePath s l ch '[n], BuildNode ch l1) =>
    BuildChildren (NodeT t n (ch:chs)) l
  where {}

class SubStructurePath s0 l0 s l | s0 l0 l -> s
  where {}

instance
    SubStructurePath (NodeT t n (ch ': chs)) (n ': l) ch l
  where {}

instance
    SubStructurePath (NodeT t n chs) l0 s l =>
    SubStructurePath (NodeT t n (ch ': chs)) l0 s l
  where {}


--class (SubStructurePath s l sThis '[], BuildChildren sThis l) => BuildNode s l
--  where {}
class BuildNode s l
  where {}

type Build n = (BuildNode '[n] (Structure n))



-- 最終フィンブル、無凸フィンブル、プニル、玄武斧、玄武拳
-- ミュル、アン槍orヴァジラ拳、リヴァ短剣、鶴丸、天司


