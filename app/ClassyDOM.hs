{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
{-# LANGUAGE AllowAmbiguousTypes #-}

module
    ClassyDOM
where

import GHC.Exts
import GHC.TypeLits
import Data.Proxy
import qualified Data.Tree as Tr
import qualified Data.Text as Tx

-- Types
data Attr = Attr
  {
    attrClass :: [Tx.Text],
    attrAttr :: [(Tx.Text, Tx.Text)]
  }

data NodeV = NodeV Tx.Text Attr | TextNode Tx.Text

type TreeV = Tr.Tree NodeV

type Selector = [Tx.Text] -- reverse ordered, for efficiency
selectorStr :: Selector -> Tx.Text
selectorStr = Tx.intercalate ">" . reverse

data CSSStyleEntry = CSSStyleEntry
  {
    styleName :: Tx.Text,
    styleValue :: Tx.Text
  }

data CSSEntry = CSSEntry
  {
    cssSelector :: Selector,
    cssStyle :: [Tx.Text]
  }

-- Type kinds
data TreeT = NodeT Symbol Symbol [TreeT] | TextT

--
-- Classes (Implemented by users)
--
class Template (name :: Symbol)
  where
    type Data name :: *
    type Structure name :: TreeT

class BuildNode tmpl n
  where
    css :: [CSSStyleEntry]
    buildNode :: Data tmpl -> Builder tmpl n


--
-- SubTree
--
type family
    SubTreeHelper
        (x :: TreeT) (xs :: [TreeT]) (xss :: [[TreeT]]) (n :: Symbol)
        :: TreeT
  where
    SubTreeHelper (NodeT t n ch) xs xss n = NodeT t n ch
    SubTreeHelper (NodeT t n0 (ch : chs)) xs xss n =
        SubTreeHelper ch chs (xs:xss) n
    SubTreeHelper z (x:xs) xss n = SubTreeHelper x xs xss n
    SubTreeHelper z '[] (xs:xss) n = SubTreeHelper z xs xss n

type SubTree s n = SubTreeHelper s '[] '[] n

--
-- Builder
--
data BuilderElem_ (tmpl :: Symbol) (x :: TreeT)
  where
    NodeBuilder ::
        (x ~ NodeT t n children, BuildNode tmpl n) =>
        Proxy n -> BuilderElem_ tmpl x
    TextBuilder ::
        Tx.Text -> BuilderElem_ tmpl TextT

type BuilderElem tmpl x = forall xs. Builder_ tmpl xs -> Builder_ tmpl (x:xs)

data Builder_ (tmpl :: Symbol) (xs :: [TreeT])
  where
    NilBuilder ::
        Builder_ tmpl '[]
    ConsBuilder ::
        BuilderElem_ tmpl x -> Builder_ tmpl xs -> Builder_ tmpl (x:xs)

data Builder (tmpl :: Symbol) (n :: Symbol)
  where
    Builder ::
        (SubTree (Structure tmpl) n ~ NodeT t n children, KnownSymbol n) =>
        Attr -> Builder_ tmpl children -> Builder tmpl n

buildChild ::
    forall tmpl n x t ch.
    (x ~ NodeT t n ch, BuildNode tmpl n) =>
    BuilderElem tmpl x
buildChild = ConsBuilder (NodeBuilder (Proxy @ n))

buildText ::
    Tx.Text -> BuilderElem tmpl TextT
buildText text = ConsBuilder (TextBuilder text)

--
-- BuildTmpl
--
type BuildTmpl tmpl = BuildNode  tmpl tmpl

buildTmpl :: forall tmpl. BuildTmpl tmpl => Data tmpl -> TreeV
buildTmpl d =
    breakBuilder (buildNode @tmpl @tmpl d)
  where
    breakBuilder :: forall n. Builder tmpl n -> TreeV
    breakBuilder (Builder attr bs) =
        Tr.Node (NodeV (Tx.pack $ symbolVal (Proxy @n)) attr) $ breakBuilder_ bs

    breakBuilder_ :: forall n. Builder_ tmpl n -> [TreeV]
    breakBuilder_ NilBuilder = []
    breakBuilder_ (ConsBuilder elem rest) = breakElem elem : breakBuilder_ rest

    breakElem :: forall x. BuilderElem_ tmpl x -> TreeV
    breakElem (NodeBuilder (_ :: Proxy ch)) =
        breakBuilder (buildNode @tmpl @ch d)
    breakElem (TextBuilder txt) =
        Tr.Node (TextNode txt) []

--
-- makeCSSEntry
--
class BuildNodeAll (ts :: [Symbol]) (ns :: [Symbol])
  where
    makeCSSEntry_ :: [CSSEntry]

instance BuildNodeAll '[] '[]
  where
    makeCSSEntry_ = []

instance (BuildNode t n, BuildNodeAll ts ns) => BuildNodeAll (t:ts) (n:ns)
  where
    makeCSSEntry_ = undefined

type family DataAll (ts :: [Symbol]) :: [*]

makeCSSEntry :: forall ts. BuildNodeAll ts ts => [CSSEntry]
makeCSSEntry = makeCSSEntry_ @ts @ts

{-
instance (BuildTmpl d x, BuildTmplAll d xs) => BuildTmplAll d (x:xs)
  where
    makeCSSEntry_ = traverseCSS @Structure x []
-}

{-
traverseCSS :: forall (tmpl :: TreeT) d. Selector -> CSSEntry
traverseCSS = undefined
-}

{-
type family BuildTmplAll (d :: *) (tmpls :: [Symbol]) :: Constraint
  where
    BuildTmplAll d '[] = ()
    BuildTmplAll d (x:xs) = (BuildTmpl d x, BuildTmplAll d xs)
-}
