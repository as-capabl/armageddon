{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
data NodeT = NodeT TagKind Symbol | TextT Symbol

-- Classes
class Template (name :: Symbol)
  where
    type Structure name :: Tr.Tree NodeT




-- 最終フィンブル、無凸フィンブル、プニル、玄武斧、玄武拳
-- ミュル、アン槍orヴァジラ拳、リヴァ短剣、鶴丸、天司


