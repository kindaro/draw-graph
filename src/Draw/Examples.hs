module Draw.Examples where

import Protolude
import qualified Data.List.Split as List
import Data.Bitraversable
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Graph.Inductive (DynGraph)

import qualified Layout
import Draw
import Analyze
import Instances
import Examples

renderOne :: (Back b, _) => (Text, AnyGraph) -> Diagram b
renderOne (name, AnyGraph{..}) = graph
                       & Layout.circular
                       & \ graph' -> decorateLaidOutGraph (name, graph')

decorateLaidOutGraph :: ( DynGraph graph, Bicontainer graph, Bitraversable graph
                        , Eq (graph (vertex, V2 Double) edge), IndexL graph ~ Int, Back b )
                     => (Text, graph (vertex, V2 Double) edge) -> Diagram b
decorateLaidOutGraph (name, graph) = graph
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
