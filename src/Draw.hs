module Draw where

import Protolude hiding (local)
import Diagrams.Prelude
import Diagrams.TwoD.Arrowheads
import Data.Graph.Inductive (DynGraph, Node, gmap)
import qualified Data.Graph.Inductive as Graph
import Data.Bifunctor
import Data.Bifoldable
import qualified Data.List as List
import Diagrams.TwoD.Text

import Instances
import Analyze

classify :: Eq a => [a] -> [[a]]
classify = classifyBy (==)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy eq = List.foldl' f [ ]
  where
    f [ ] y = [[y]]
    f (xs@ (x: _): xss) y | x `eq` y  = (y: xs): xss
                          | otherwise = xs: f xss y

type Back b = (V b ~ V2, N b ~ Double, Renderable (Path V2 Double) b
                , Renderable (Diagrams.TwoD.Text.Text Double) b)

draw :: forall a b gr e. (DynGraph gr, Back b, Bifoldable gr, Bicontainer gr, IndexL gr ~ Int)
     => gr (a, V2 Double) e -> Diagram b
draw graph = graph
           & biindex
           & fairMap ((\ x@(edgesIn, identifier, ((_, v), identifier'), edgesOut) -> ((isLeaf x, v), identifier')) . fst)
           & bifoldMap nodeAt (const mempty)
           & drawMore addArrow arrows
           & drawMore addLine lines
           & lwL 0.02

  where
    node :: Back b => Bool -> Node -> Diagram b
    node x identifier =
      let n = fromIntegral (Graph.order graph)
          r = sqrt $ (1 * grade) / (pi * n)
          grade = 0.1
      in scale r
            $ ( fc white . scale 2 . text . show) identifier
            <> (circle 1 & (if x then fc red else fc black) & lw none)

    nodeAt :: Back b => ((Bool, V2 Double), Node) -> Diagram b
    nodeAt ((x, v), identifier) = translate v (node x identifier & named identifier)

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
