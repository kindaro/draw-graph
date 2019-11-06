module Analyze where

import Prelude
import Data.Graph.Inductive
import Instances
import Converge

isPlanar :: Graph gr => gr a b -> Bool
isPlanar _ = undefined

isAcyclic :: (DynGraph gr, Eq (gr a b)) => gr a b -> Bool
isAcyclic gr = case (fixp tearLeaves) gr of
    Empty -> True
    _     -> False

tearLeaves :: DynGraph gr => gr a b -> gr a b
tearLeaves gr = let f x = if isLeaf x then id else (x:) in subgraph ((fmap node' . ufold f [ ]) gr) gr

isLeaf :: Context a b -> Bool
isLeaf (_, _, _, [ ]) = True
isLeaf (_, _, _, _  ) = False
