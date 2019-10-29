module Main where

import Protolude
import qualified Data.List       as List
import qualified Data.List.Split as List
import Data.Bifunctor
import Data.Bitraversable
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text

import qualified Layout
import Draw
import Analyze
import Instances
import Examples


main :: IO ()
main = mainWith @(Diagram B)
     $ (tile . fmap renderOne) examples

renderOne :: AnyGraph -> Diagram B
renderOne AnyGraph{..} = graph
                       & Layout.circular
                       & draw
                       & decorate
  where
    decorate x = (frame 0.2 . vcat . fmap (frame 0.1))
        [ hcat blips
        , x <> (square 2.2 & lwL 0.01)
        , (rect 2.2 0.3 & lw none) <> (scale 0.3 . text . toS) name
        ]

    blips =
      [ blip (isAcyclic graph)
      ]

    blip x = square 0.2
           & lwL 0.01 
           & fcA if x
                    then lime `withOpacity` 0.8
                    else grey `withOpacity` 0.1

tile :: [Diagram B] -> Diagram B
tile xs = let columns = (ceiling . sqrt . fromIntegral . length) xs
          in (vcat . fmap hcat . List.chunksOf columns) xs
