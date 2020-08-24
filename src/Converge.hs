module Converge where

import Prelude
import qualified Data.List as List
import Data.Function
import Data.Ord

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

classify :: Eq a => [a] -> [[a]]
classify = classifyBy (==)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy eq = List.foldl' f [ ]
  where
    f [ ] y = [[y]]
    f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
                          | otherwise = xs: f xss y

classifyOn ∷ Ord π ⇒ (a → π) → [a] → [[a]]
classifyOn f = (map . map) snd . List.groupBy ((==) `on` fst) . List.sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
