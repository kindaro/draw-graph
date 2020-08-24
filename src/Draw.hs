module Draw where

import Protolude hiding (local)
import Prelude.Unicode ((≠), (≡))
import Diagrams.Prelude
import Diagrams.TwoD.Arrowheads
import Data.Graph.Inductive (DynGraph, Node, gmap)
import qualified Data.Graph.Inductive as Graph
import Data.Bifunctor
import Data.Function
import Data.Bifoldable
import qualified Data.Map as Map
import qualified Data.List as List
import Diagrams.TwoD.Text

import Instances
import Analyze
import Converge

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

    swapEquality (x, y) (x', y')
        | x == y' && y == x' = True
        | otherwise = False

    drawMore = fmap flip List.foldl'

-- I need to do something else. I should draw everything with a single
-- bifold. But for that I first need to conflate edges and add labels that tell
-- me which edges go both ways.

data EdgeType = Arrow | Line deriving (Eq, Ord, Show)

data U2 a = U2_ {left, right ∷ a} deriving (Eq, Ord, Show)

unorderedPair ∷ Ord a ⇒ a → a → U2 a
unorderedPair x y
  | x < y = U2_ {left = x, right = y}
  | otherwise = U2_ {left = y, right = x}

pattern U2 ∷ Ord a ⇒ a → a → U2 a
pattern U2 x y ← U2_ x y
  where U2 x y = unorderedPair x y

v2ToU2 ∷ Ord a ⇒ V2 a → U2 a
v2ToU2 (V2 x y) = U2 x y

u2ToV2 ∷ Ord a ⇒ U2 a → V2 a
u2ToV2 (U2 x y) = V2 x y

-- | The idea here is that there is at most one edge between any two nodes of a
-- compactified graph, going in the ascending direction, and that it has the summa
-- of edge labels previously going there and back as its label.
compactifyEdges ∷ _ ⇒ graph nodeLabel edgeLabel → graph (nodeLabel, [edgeLabel]) ([edgeLabel], [edgeLabel])
compactifyEdges graph =
  let edges = fmap (unTidyEdge . first u2ToV2) . Map.toList . edgeBundles $ graph
      nodes = fmap (associateLoopLabelsWithNode graph) . Graph.labNodes $ graph
  in Graph.mkGraph nodes edges
  where
    associateLoopLabelsWithNode ∷ _ ⇒ graph nodeLabel edgeLabel → (Node, nodeLabel) → (Node, (nodeLabel, [edgeLabel]))
    associateLoopLabelsWithNode graph (n, z) = (n, (z, fromMaybe [ ] (Map.lookup n (loops graph))))

-- | This one inverts `compactifyEdges`.`
expandEdges ∷ _ ⇒ graph (nodeLabel, [edgeLabel]) ([edgeLabel], [edgeLabel]) → graph nodeLabel edgeLabel
expandEdges graph =
  let loops = concat . fmap expandLoops . Graph.labNodes $ graph
      edges = concat . fmap expandEdges . Graph.labEdges $ graph
      nodes = fmap (fmap fst) . Graph.labNodes $ graph
  in Graph.mkGraph nodes (loops ++ edges)
  where
    expandLoops ∷ (Node, (nodeLabel, [edgeLabel])) → [(Node, Node, edgeLabel)]
    expandLoops (n, (z, labels)) = fmap (n, n, ) labels

    expandEdges ∷ (Node, Node, ([edgeLabel], [edgeLabel])) → [(Node, Node, edgeLabel)]
    expandEdges (x, y, (us, vs)) = fmap (x, y, ) us ++ fmap (y, x, ) vs

edgeBundles :: _ ⇒ graph nodeLabel edgeLabel → Map (U2 Node) ([edgeLabel], [edgeLabel])
edgeBundles = Map.fromListWith biappend . fmap (bimap (v2ToU2 . fst) discriminate . diag) .  labEdges' . dropLoops

-- | Sort edges that go in ascending node number direction to the left and descending to the right; drop loops altogether.
discriminate ∷ (V2 Node, edgeLabel) → ([edgeLabel], [edgeLabel])
discriminate (V2 x y, z)
  | x < y = ([z], [ ])
  | x > y = ([ ], [z])
  | x ≡ y = ([ ], [ ])

labEdges' ∷ _ ⇒ graph nodeLabel edgeLabel → [(V2 Node, edgeLabel)]
labEdges' = fmap tidyEdge . Graph.labEdges

dropLoops ∷ _ ⇒ graph nodeLabel edgeLabel → graph nodeLabel edgeLabel
dropLoops = uncurry Graph.mkGraph . bimap Graph.labNodes (filter (\(x, y, z) → x ≠ y) . Graph.labEdges) . diag

loops ∷ _ ⇒ graph nodeLabel edgeLabel → Map Node [edgeLabel]
loops = Map.fromListWith (++) . fmap (bimap (\(V2 x y) → x) pure) . filter (\(V2 x y, z) → x ≡ y) . labEdges'

tidyEdge ∷ (Node, Node, edgeLabel) → (V2 Node, edgeLabel)
tidyEdge (x, y, z) = (V2 x y, z)

unTidyEdge ∷ (V2 Node, edgeLabel) → (Node, Node, edgeLabel)
unTidyEdge (V2 x y, z) = (x, y, z)

biappend ∷ ([a], [b]) → ([a], [b]) → ([a], [b])
biappend (xs, ys) (xs', ys') = (xs <> xs', ys <> ys')

unsafeHead = headDef (panic "`classifyBy` returns a list of non-empty lists.")
