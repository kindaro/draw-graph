{-# options_ghc -fno-warn-orphans #-}

module Instances where

import Protolude
import Data.Bifoldable
import Data.Bitraversable
import Data.Graph.Inductive (Graph, DynGraph, Node, Context, Decomp, Gr)
import qualified Data.Graph.Inductive as Graph
import Control.Comonad.Store

insert :: DynGraph gr => Context a b -> gr a b -> gr a b
insert = (Graph.&)

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

class Bicontainer c where
    type IndexL c
    type IndexR c
    biindex :: c l r -> c (l, IndexL c) (r, IndexR c)

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
