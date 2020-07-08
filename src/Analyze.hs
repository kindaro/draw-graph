module Analyze where

import Protolude
import Data.Graph.Inductive (Graph, DynGraph, Node, GDecomp, LEdge, Context, gmap)
import qualified Data.Graph.Inductive as Graph
import Instances
import Converge
import qualified Data.List as List
import System.IO.Unsafe

isPlanar :: Graph gr => gr a b -> Bool
isPlanar _ = undefined

isAcyclic :: (DynGraph gr, Eq (gr a b)) => gr a b -> Bool
isAcyclic gr = case (fixp shedLeaves) gr of
    Empty -> True
    _     -> False

autumn :: (Eq (gr a b), DynGraph gr) => gr a b -> [gr a b]
autumn = converge . iterate shedLeaves

shedLeaves :: DynGraph gr => gr a b -> gr a b
shedLeaves gr =
  let notLeaves = (fmap fst . filter (not . snd . snd) . Graph.labNodes . fairMap (fmap isLeaf . diag . fst)) gr
  in Graph.subgraph notLeaves gr

isLeaf :: Context a b -> Bool
isLeaf (_, _, _, [ ]) = True
isLeaf (_, _, _, _  ) = False
