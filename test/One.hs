module Main where

import qualified Data.List       as List
import qualified Data.List.Split as List
import Data.Bifunctor
import Data.Bitraversable
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Data.Graph.Inductive (DynGraph, Gr, Node)
import qualified Data.Graph.Inductive.Example as E

import qualified Layout
import Draw
import Analyze
import Instances

data AnyGraph = forall gr a b.
              (DynGraph gr, Bicontainer gr, Bitraversable gr, Eq (gr a b), IndexL gr ~ Int)
              => AnyGraph { getSomeGraph :: gr a b }

niceExamples :: [AnyGraph]
niceExamples = [ AnyGraph E.clr486, AnyGraph E.clr528, AnyGraph E.d3
           , AnyGraph E.e, AnyGraph E.loop
           , AnyGraph E.vor, AnyGraph E.ab, AnyGraph E.clr489, AnyGraph E.clr595
           , AnyGraph E.dag3, AnyGraph E.e3, AnyGraph E.gr1
           , AnyGraph E.cyc3
           , AnyGraph E.kin248, AnyGraph (E.star @Gr 13)
           , AnyGraph E.abb, AnyGraph E.clr479, AnyGraph E.clr508, AnyGraph E.d1
           , AnyGraph E.dag4, AnyGraph E.g3
           , AnyGraph E.g3b, AnyGraph (E.ucycle @Gr 13)
           ]

main :: IO ()
main =
  let dia = renderOne (niceExamples !! 1)
      size = mkSizeSpec (r2 (Just 1000, Just 1000))
  in renderPretty "one.svg" size dia

renderOne :: AnyGraph -> Diagram B
renderOne (AnyGraph gr) = gr & Layout.circular
                             & draw
                             & decorate gr
                             & frame
  where
    decorate gr' x = if isAcyclic gr'
                        then x <> (circle 0.1 & fc green & translate (r2 (0.95, 0.95)))
                        else x

    frame d = d <> (square 2.4 & lcA (black `withOpacity` 0)) <> square 2.2

tile :: [Diagram B] -> Diagram B
tile xs = let columns = (ceiling . sqrt . fromIntegral . length) xs
          in (vcat . fmap hcat . List.chunksOf columns) xs
