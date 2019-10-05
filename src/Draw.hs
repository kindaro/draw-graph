module Draw where

import Diagrams.Prelude
import Data.Graph.Inductive
import Data.Graph.Inductive.Example (cyc3)
import System.Random
import Control.Comonad.Store
import qualified Data.List.Split as List

layout :: Graph gr => gr a b -> gr (V2 Int, a) b
layout = undefined

randomLayout :: Graph gr => gr a b -> gr (V2 Int, a) b
randomLayout graph = undefined
  where
    randomCoordinates = randoms (mkStdGen 0)
    unsafeList2Tuple [x, y] = (x, y)
    randomPoints :: [V2 Int]
    randomPoints = let f = fmap (r2 . unsafeList2Tuple) . List.chunksOf 2 in f randomCoordinates

storeGraph :: (Graph gr) => gr a b -> Node -> Store Node (Decomp gr a b)
storeGraph graph node = let f identifier = match identifier graph
                        in store f node
