module Draw where

import Diagrams.Prelude
import Data.Graph.Inductive (Graph, Node)
import qualified Data.Graph.Inductive as Graph
import Data.Bifoldable
import qualified Data.List as List

import Instances

type Back b = (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b)

draw :: (Graph gr, Back b, Bifoldable gr, Bicontainer gr, IndexL gr ~ Int)
     => gr (a, V2 Double) e -> Diagram b
draw graph =
  let vertices = bifoldMap nodeAt (const mempty) . biindex $ graph
  in lwL 0.02 $ List.foldl' (\d (idFrom, idTo) -> d & arrow idFrom idTo) vertices (Graph.edges graph)

  where
    node :: Back b => Diagram b
    node = let n = fromIntegral (Graph.size graph)
               r = sqrt $ (1 * grade) / (pi * n)
               grade = 0.1
           in circle r

    nodeAt :: Back b => ((a, V2 Double), Node) -> Diagram b
    nodeAt ((_, v), identifier) = translate v (node & named identifier)

    arrow = connectOutside' (with & headLength .~ local 0.1)
