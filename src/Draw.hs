module Draw where

import Diagrams.Prelude
import Data.Graph.Inductive (Graph)
import qualified Data.Graph.Inductive as Graph
import Data.Bifoldable

type Back b = (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b)

draw :: (Graph gr, Back b, Bifoldable gr) => gr (V2 Double, a) e -> Diagram b
draw graph = bifoldMap (nodeAt . fst) (const mempty) graph

  where
    node :: Back b => Diagram b
    node = let n = fromIntegral (Graph.size graph)
               r = sqrt $ (1 * grade) / (pi * n)
               grade = 0.1
           in circle r

    nodeAt :: Back b => V2 Double -> Diagram b
    nodeAt v = translate v node
