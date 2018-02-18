{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

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
data TreeT = NodeT TagKind Symbol [TreeT] | TextT

-- Classes
class Template (name :: Symbol)
  where
    type Structure name :: TreeT


type family SubTreeHelper (x :: TreeT) (xs :: [TreeT]) (xss :: [[TreeT]]) (n :: Symbol) :: TreeT
  where
    SubTreeHelper (NodeT t n ch) xs xss n = NodeT t n ch
    SubTreeHelper (NodeT t n0 (ch : chs)) xs xss n = SubTreeHelper ch chs (xs:xss) n
    SubTreeHelper z (x:xs) xss n = SubTreeHelper x xs xss n
    SubTreeHelper z '[] (xs:xss) n = SubTreeHelper z xs xss n

type SubTree s n = SubTreeHelper s '[] '[] n

data BuilderElem (d :: *) (tmpl :: Symbol) (x :: TreeT)
  where
    NodeBuilder :: (x ~ NodeT t n children, BuildNode d tmpl n) => BuilderElem d tmpl x
    TextBuilder :: Tx.Text -> BuilderElem d tmpl TextT

data Builder_ (d :: *) (tmpl :: Symbol) (xs :: [TreeT])
  where
    NilBuilder :: Builder_ d tmpl '[]
    ConsBuilder :: BuilderElem d tmpl x -> Builder_ d tmpl xs -> Builder_ d tmpl (x:xs)

data Builder (d :: *) (tmpl :: Symbol) (n :: Symbol)
  where
    Builder ::
        SubTree (Structure tmpl) n ~ NodeT t n children =>
        Builder_ d tmpl children -> Builder d tmpl n

class BuildNode d tmpl n
  where
    buildNode :: d -> Builder d tmpl n

