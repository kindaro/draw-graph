module Main where

import Protolude
import qualified Data.List       as List
import qualified Data.List.Split as List
import Data.Bifunctor
import Data.Bitraversable
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified Layout
import Draw
import Analyze
import Instances
import Examples


main :: IO ()
main = mainWith @(Diagram B)
     $ (tile . fmap renderOne) examples

renderOne :: AnyGraph -> Diagram B
renderOne AnyGraph{..} = graph & Layout.circular
                               & draw
                               & decorate graph
                               & frame
  where
    decorate graph' x = if isAcyclic graph'
                           then x <> (circle 0.1 & fc green & translate (r2 (0.95, 0.95)))
                           else x

    frame d = d <> (square 2.4 & lcA (black `withOpacity` 0)) <> square 2.2

tile :: [Diagram B] -> Diagram B
tile xs = let columns = (ceiling . sqrt . fromIntegral . length) xs
          in (vcat . fmap hcat . List.chunksOf columns) xs
