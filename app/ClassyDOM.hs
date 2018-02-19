{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module
    ClassyDOM
where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Tree as Tr
import qualified Data.Text as Tx

-- Types
data TagKind = A | Div | Span

data Attr = Attr
  {
    attrClass :: [Tx.Text],
    attrAttr :: [(Tx.Text, Tx.Text)]
  }

data NodeV = NodeV Tx.Text Attr | TextNode Tx.Text

type TreeV = Tr.Tree NodeV

data CSSStyleEntry = CSSStyleEntry
  {
    styleName :: Tx.Text,
    styleValue :: Tx.Text
  }

data CSSEntry = CSSEntry
  {
    cssSelector :: Tx.Text,
    cssStyle :: [Tx.Text]
  }

-- Type kinds
data TreeT = NodeT Symbol Symbol [TreeT] | TextT

-- Classes
class Template (name :: Symbol)
  where
    type Structure name :: TreeT


--
-- SubTree
--
type family
    SubTreeHelper
        (x :: TreeT) (xs :: [TreeT]) (xss :: [[TreeT]]) (n :: Symbol)
        :: TreeT
  where
    SubTreeHelper (NodeT t n ch) xs xss n = NodeT t n ch
    SubTreeHelper (NodeT t n0 (ch : chs)) xs xss n = SubTreeHelper ch chs (xs:xss) n
    SubTreeHelper z (x:xs) xss n = SubTreeHelper x xs xss n
    SubTreeHelper z '[] (xs:xss) n = SubTreeHelper z xs xss n

type SubTree s n = SubTreeHelper s '[] '[] n

--
-- Builder
--

data BuilderElem_ (d :: *) (tmpl :: Symbol) (x :: TreeT)
  where
    NodeBuilder :: (x ~ NodeT t n children, BuildNode d tmpl n) => Proxy n -> BuilderElem_ d tmpl x
    TextBuilder :: Tx.Text -> BuilderElem_ d tmpl TextT

type BuilderElem d tmpl x = forall xs. Builder_ d tmpl xs -> Builder_ d tmpl (x:xs)

data Builder_ (d :: *) (tmpl :: Symbol) (xs :: [TreeT])
  where
    NilBuilder :: Builder_ d tmpl '[]
    ConsBuilder :: BuilderElem_ d tmpl x -> Builder_ d tmpl xs -> Builder_ d tmpl (x:xs)

data Builder (d :: *) (tmpl :: Symbol) (n :: Symbol)
  where
    Builder ::
        (SubTree (Structure tmpl) n ~ NodeT t n children, KnownSymbol n) =>
        Attr -> Builder_ d tmpl children -> Builder d tmpl n

buildChild ::
    forall d tmpl n x t ch.
    (x ~ NodeT t n ch, BuildNode d tmpl n) =>
    BuilderElem d tmpl x
buildChild = ConsBuilder (NodeBuilder (Proxy @ n))

buildText ::
    Tx.Text -> BuilderElem d tmpl TextT
buildText text = ConsBuilder (TextBuilder text)

--
-- BuildNode
--
class BuildNode d tmpl n
  where
    css :: Proxy d -> Proxy tmpl -> Proxy n -> [CSSStyleEntry]
    buildNode :: Proxy tmpl -> Proxy n -> d -> Builder d tmpl n

buildTmpl :: forall tmpl d. BuildNode d tmpl tmpl => Proxy tmpl -> d -> TreeV
buildTmpl p d = breakBuilder (buildNode p p d)
  where
    breakBuilder :: forall n. Builder d tmpl n -> TreeV
    breakBuilder (Builder attr bs) =
        Tr.Node (NodeV (Tx.pack $ symbolVal (Proxy @n)) attr) $ breakBuilder_ bs

    breakBuilder_ :: forall n. Builder_ d tmpl n -> [TreeV]
    breakBuilder_ NilBuilder = []
    breakBuilder_ (ConsBuilder elem rest) = breakElem elem : breakBuilder_ rest

    breakElem :: forall x. BuilderElem_ d tmpl x -> TreeV
    breakElem (NodeBuilder p1) = breakBuilder (buildNode p p1 d)
    breakElem (TextBuilder txt) = Tr.Node (TextNode txt) []

