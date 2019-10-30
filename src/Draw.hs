module Draw where

import Protolude hiding (local)
import Diagrams.Prelude
import Diagrams.TwoD.Arrowheads
import Data.Graph.Inductive (Graph, Node)
import qualified Data.Graph.Inductive as Graph
import Data.Bifunctor
import Data.Bifoldable
import qualified Data.List as List

import Instances

classify :: Eq a => [a] -> [[a]]
classify = classifyBy (==)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy eq = List.foldl' f [ ]
  where
    f [ ] y = [[y]]
    f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
                          | otherwise = xs: f xss y

type Back b = (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b)

draw :: (Graph gr, Back b, Bifoldable gr, Bicontainer gr, IndexL gr ~ Int)
     => gr (a, V2 Double) e -> Diagram b
draw graph = graph
           & biindex
           & bifoldMap nodeAt (const mempty)
           & drawMore addArrow arrows
           & drawMore addLine lines
           & lwL 0.02

  where
    node :: Back b => Diagram b
    node = let n = fromIntegral (Graph.order graph)
               r = sqrt $ (1 * grade) / (pi * n)
               grade = 0.1
           in circle r

    nodeAt :: Back b => ((a, V2 Double), Node) -> Diagram b
    nodeAt ((_, v), identifier) = translate v (node & named identifier)

    addArrow, addLine :: Back b => Diagram b -> (Node, Node) -> Diagram b
    addArrow d (idFrom, idTo) = d & connectOutside' (with & headLength .~ local 0.1) idFrom idTo
    addLine  d (idFrom, idTo) = d & connectOutside' (with & arrowHead .~ noHead)     idFrom idTo

    arrows, lines :: [(Node, Node)]
    (arrows, lines) = Graph.edges graph
                    & classifyBy swapEquality
                    & List.partition ((== 1) . length) 
                    & bimap (fmap unsafeHead) (fmap unsafeHead)
      where
        unsafeHead = headDef (panic "`classifyBy` returns a list of non-empty lists.")

    swapEquality (x, y) (x', y')
        | x == x' && y == y' = True
        | x == y' && y == x' = True
        | otherwise = False

    drawMore = fmap flip List.foldl'
