{-# options_ghc -fno-warn-orphans #-}

module Instances where

import Protolude
import Data.Bifoldable
import Data.Bitraversable
import Data.Graph.Inductive (Graph, DynGraph, Node, Context, Decomp, GDecomp, Gr)
import qualified Data.Graph.Inductive as Graph
import Control.Comonad.Store

insert :: DynGraph gr => Context a b -> gr a b -> gr a b
insert = (Graph.&)

getNodeFromContext :: Context vertex edge -> Node
getNodeFromContext (_, x, _, _) = x

mapContext :: (a -> b) -> Context a edge -> Context b edge
mapContext f (edgesIn, identifier, label, edgesOut) = (edgesIn, identifier, f label, edgesOut)

pattern Anywhere :: DynGraph gr => Graph.Context a b -> gr a b -> gr a b
pattern Anywhere context remainingGraph <- (Graph.matchAny -> (context, remainingGraph))
    where Anywhere context remainingGraph = insert context remainingGraph

pattern Empty :: Graph gr => gr a b
pattern Empty <- (Graph.isEmpty -> True)
    where Empty = Graph.empty

{-# complete Anywhere, Empty :: Gr #-}

instance Bifoldable Gr where
    bifoldr _ _ zero Empty = zero
    bifoldr mergeVertex mergeEdge zero (Anywhere (edgesIn, _, label, edgesOut) r) = 
      let mergeAdj adj zero' = foldr (mergeEdge . fst) zero' adj
          f = mergeAdj edgesOut . mergeAdj edgesIn . mergeVertex label
      in bifoldr mergeVertex mergeEdge (f zero) r

instance Bitraversable Gr where
    bitraverse _ _ Empty = pure Empty
    bitraverse effectVertex effectEdge (Anywhere (edgesIn, identifier, label, edgesOut) r) =
      let traverseEdges = traverse (bitraverse effectEdge identity . fmap pure)
      in do
        label' <- effectVertex label
        edgesIn'  <- traverseEdges edgesIn
        edgesOut' <- traverseEdges edgesOut
        r' <- bitraverse effectVertex effectEdge r
        return $ insert (edgesIn', identifier, label', edgesOut') r'

storeGraph :: (Graph gr) => gr a b -> Node -> Store Node (Decomp gr a b)
storeGraph graph node = let f identifier = Graph.match identifier graph
                        in store f node

class Container c where
  type Index c
  index :: c i -> c (i, Index c)

class Bicontainer c where
    type IndexL c
    type IndexR c
    biindex :: c l r -> c (l, IndexL c) (r, IndexR c)
    lindex :: c l r -> c (l, IndexL c) r
    rindex :: c l r -> c l (r, IndexR c)

instance Bicontainer Gr where
    type IndexL Gr = Node
    type IndexR Gr = (Node, Node)

    biindex Empty = Empty
    biindex (Anywhere (edgesIn, identifier, label, edgesOut) r) =
      let indexEdgeIn  (x, from) = ((x, (from, identifier)), from)
          indexEdgeOut (x, to)   = ((x, (identifier, to  )), to  )
          label' = (label, identifier)
          edgesIn'  = fmap indexEdgeIn  edgesIn
          edgesOut' = fmap indexEdgeOut edgesOut
          r' = biindex r
      in insert (edgesIn', identifier, label', edgesOut') r'

    lindex = bimap identity fst . biindex
    rindex = bimap fst identity . biindex

newtype Flip f a b = Flip { flop :: f b a }

newtype Blip f a b = Blip { blop :: f a b }

instance Bicontainer c => Container (Blip c a) where
  type Index (Blip c a) = IndexR c
  index = Blip . rindex . blop

instance Bicontainer c => Container (Flip c a) where
  type Index (Flip c a) = IndexL c
  index = Flip . lindex . flop

data PointedGraph gr edge vertex = PointedGraph
  { context :: Context vertex edge
  , remainder :: gr vertex edge
  }
  -- Isomorphic to `GDecomp` — witnessed by `compose` and `decompose`.

instance DynGraph gr => Functor (PointedGraph gr edge) where
  fmap f PointedGraph {..} = PointedGraph { context =mapContext f context, remainder = Graph.nmap f remainder }

chooseArbitraryFocus :: DynGraph gr => gr vertex edge -> Maybe (PointedGraph gr edge vertex)
chooseArbitraryFocus Empty = Nothing
chooseArbitraryFocus (Anywhere context remainder) = Just PointedGraph {..}

compose :: DynGraph gr => GDecomp gr vertex edge -> PointedGraph gr edge vertex
compose (context, remainder) = PointedGraph {..}

decompose :: Graph gr => PointedGraph gr edge vertex -> GDecomp gr vertex edge
decompose PointedGraph {..} = (context, remainder)

refocus :: DynGraph gr => PointedGraph gr edge vertex -> Node -> PointedGraph gr edge vertex
refocus x@PointedGraph {..} n = case Graph.match n remainder of
  (Nothing, _) -> x
  (Just _, _) -> case Graph.match n (insert context remainder) of
    (Nothing, _) -> panic "Unreachable. If a node is in a subgraph, it is also in the larger graph."
    (Just context', remainder') -> compose (context', remainder')

-- foldOverDecomp :: DynGraph gr
--   => (GDecomp gr vertex edge -> accumulator -> accumulator)
--   -> accumulator -> gr vertex edge -> accumulator
-- foldOverDecomp _ x Empty = x
-- foldOverDecomp f x (Anywhere context remainder) = f (context, remainder) (foldOverDecomp f x remainder)

instance DynGraph gr => Comonad (PointedGraph gr edge) where
  extract PointedGraph {context = (_, _, label, _), ..} = label

  extend f x = case extend' x f (uncurry insert (decompose x)) of
    Empty -> panic "Unreachable. There is at least one node in the source and nodes are never dropped."
    Anywhere context' remainder' -> refocus (compose (context', remainder')) (getNodeFromContext (context x))

    where
    extend' whole f Empty = Empty
    extend' whole f (Anywhere context remainder) =
      let
        newLabel = f (refocus whole (getNodeFromContext context))
        newContext = mapContext (const newLabel) context
      in Anywhere newContext (extend' whole f remainder)

-- fairMap :: forall gr vertex edge. DynGraph gr => (GDecomp gr vertex edge -> vertex') -> gr vertex edge -> gr vertex' edge
-- fairMap f = foldOverDecomp (insert . g) Empty
--   where
--     g :: GDecomp gr vertex edge -> Context vertex edge
--     g x@(context, remainder) = mapContext (const (f x)) context

fairMap :: forall gr a b edge. DynGraph gr => (GDecomp gr a edge -> b) -> gr a edge -> gr b edge
fairMap f Empty = Empty
fairMap f (Anywhere context remainder) = uncurry insert . decompose . extend (f . decompose) $ PointedGraph {..}
