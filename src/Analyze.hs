module Analyze where

import Prelude
import Data.Graph.Inductive
import Instances

converge :: Eq a => [a] -> [a]
converge = convergeBy (==)

fixp :: Eq a => (a -> a) -> a -> a
fixp f = last . converge . iterate f

convergeBy :: (a -> a -> Bool) -> [a] -> [a]
convergeBy _ [ ] = [ ]
convergeBy _ [x] = [x]
convergeBy eq (x: xs@(y: _))
    | x `eq` y = [x]
    | otherwise = x : convergeBy eq xs

fixpBy :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpBy eq f = last . convergeBy eq . iterate f

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
