module Converge where

import Prelude

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

diag :: a -> (a, a)
diag x = (x, x)
