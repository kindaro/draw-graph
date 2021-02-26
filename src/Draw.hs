module Draw where

import Protolude hiding (local)
import Prelude.Unicode ((≠), (≡))
import Diagrams.Prelude
import Diagrams.TwoD.Arrowheads
import Data.Graph.Inductive (Graph, DynGraph, Node, gmap)
import qualified Data.Graph.Inductive as Graph
import Data.Bifunctor
import Data.Function
import Data.Bifoldable
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
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
draw = draw' defaultNodeStyle defaultArrowStyle 0.02

draw' :: forall a b gr e. (DynGraph gr, Back b, Bifoldable gr, Bicontainer gr, IndexL gr ~ Int)
     => (Bool -> Double -> Node -> Diagram b) -> ArrowOpts Double → Double -> gr (a, V2 Double) e -> Diagram b
draw' node arrow thickness graph = graph
           & biindex
           & fairMap ((\ x@(edgesIn, identifier, ((_, v), identifier'), edgesOut) -> ((isLeaf x, v), identifier')) . fst)
           & bifoldMap nodeAt (const mempty)
           & drawMore addArrow arrows
           & drawMore addLine lines
           & lwL thickness

  where
    nodeAt :: Back b => ((Bool, V2 Double), Node) -> Diagram b
    nodeAt ((x, v), identifier) = translate v (node x (fromIntegral (Graph.order graph)) identifier & named identifier)

    addArrow, addLine :: Back b => Diagram b -> (Node, Node) -> Diagram b
    addArrow d (idFrom, idTo) = d & connectOutside' arrow idFrom idTo
    addLine  d (idFrom, idTo) = d & connectOutside' (arrow & arrowHead .~ noHead) idFrom idTo

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

defaultNodeStyle :: Back b => Bool -> Double -> Node -> Diagram b
defaultNodeStyle x nodeScale identifier =
  let r = sqrt $ (1 * grade) / (pi * nodeScale)
      grade = 0.1
  in scale r
        $ ( fc white . scale 2 . text . show) identifier
        <> (circle 1 & (if x then fc red else fc black) & lw none)

defaultArrowStyle ∷ ArrowOpts Double
defaultArrowStyle = with & headLength .~ local 0.1

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

swapV2 ∷ V2 a → V2 a
swapV2 (V2 x y) = V2 y x

type CompactNode nodeLabel edgeLabel = (nodeLabel, MultiSet edgeLabel)
type CompactEdge edgeLabel = (MultiSet edgeLabel, MultiSet edgeLabel)
type CompactGraph graph nodeLabel edgeLabel = graph (CompactNode nodeLabel edgeLabel) (CompactEdge edgeLabel)

-- | The idea here is that there is at most one edge between any two nodes of a
-- compactified graph, going in the ascending direction, and that it has the summa
-- of edge labels previously going there and back as its label.
compactifyEdges ∷ _ ⇒ graph nodeLabel edgeLabel → CompactGraph graph nodeLabel edgeLabel
compactifyEdges graph =
  let edges = fmap (unTidyEdge . first u2ToV2) . Map.toList . edgeBundles $ graph
      nodes = fmap (associateLoopLabelsWithNode graph) . Graph.labNodes $ graph
  in Graph.mkGraph nodes edges
  where
    associateLoopLabelsWithNode graph (n, z) = (n, (z, fromMaybe MultiSet.empty (Map.lookup n (loops graph))))

-- | This one inverts `compactifyEdges`.`
expandEdges ∷ DynGraph graph ⇒ CompactGraph graph nodeLabel edgeLabel → graph nodeLabel edgeLabel
expandEdges graph =
  let
    loops = concatMap (uncurry (fmap . (,)) . bimap (uncurry V2 . diag) (MultiSet.toList . snd)) . Graph.labNodes $ graph
    edges = concatMap (uncurry (\n (there, back) → fmap (n,) (MultiSet.toList there) ++ fmap (swapV2 n, ) (MultiSet.toList back))) . labEdges' $ graph
    nodes = fmap (fmap fst) . Graph.labNodes $ graph
  in mkGraph' nodes (loops ++ edges)

edgeBundles :: (Ord edgeLabel, DynGraph graph) ⇒ graph nodeLabel edgeLabel → Map (U2 Node) (CompactEdge edgeLabel)
edgeBundles = Map.fromListWith mappend . fmap (bimap (v2ToU2 . fst) discriminate . diag) .  labEdges' . dropLoops
  where
    -- | Sort edges that go in ascending node number direction to the left and descending to the right; drop loops altogether.
    discriminate ∷ (V2 Node, edgeLabel) → CompactEdge edgeLabel
    discriminate (V2 x y, z)
      | x < y = (MultiSet.singleton z, MultiSet.empty)
      | x > y = (MultiSet.empty, MultiSet.singleton z)
      | x ≡ y = (MultiSet.empty, MultiSet.empty)

labEdges' ∷ Graph graph ⇒ graph nodeLabel edgeLabel → [(V2 Node, edgeLabel)]
labEdges' = fmap tidyEdge . Graph.labEdges

insEdges' ∷ DynGraph graph ⇒ [(V2 Node, edgeLabel)] → graph nodeLabel edgeLabel → graph nodeLabel edgeLabel
insEdges' edges graph = Graph.insEdges (fmap unTidyEdge edges) graph

mkGraph' ∷ DynGraph graph ⇒ [(Node, nodeLabel)] → [(V2 Node, edgeLabel)] → graph nodeLabel edgeLabel
mkGraph' nodes edges = insEdges' edges (Graph.mkGraph nodes [ ])

dropLoops ∷ DynGraph graph ⇒ graph nodeLabel edgeLabel → graph nodeLabel edgeLabel
dropLoops = filterEdges (\(V2 n m, _) →n ≠ m)

loops ∷ (Ord edgeLabel, Graph graph) ⇒ graph nodeLabel edgeLabel → Map Node (MultiSet edgeLabel)
loops = Map.fromListWith MultiSet.union . fmap (bimap (\(V2 x _) → x) MultiSet.singleton) . filter (\(V2 x y, _) → x ≡ y) . labEdges'

expandLoops :: Map Node (MultiSet edgeLabel) → [(V2 Node, edgeLabel)]
expandLoops = fmap (first (uncurry V2 . diag)) . expandMultiMap

expandMultiMap ∷ Ord key ⇒ Map key (MultiSet value) → [(key, value)]
expandMultiMap u = Map.keys u >>= \ key → fmap (key, ) (MultiSet.toList (u Map.! key))

tidyEdge ∷ (Node, Node, edgeLabel) → (V2 Node, edgeLabel)
tidyEdge (x, y, z) = (V2 x y, z)

unTidyEdge ∷ (V2 Node, edgeLabel) → (Node, Node, edgeLabel)
unTidyEdge (V2 x y, z) = (x, y, z)

unsafeHead = headDef (panic "`classifyBy` returns a list of non-empty lists.")

filterEdges ∷ DynGraph gr ⇒ ((V2 Node, edgeLabel) → Bool) → gr nodeLabel edgeLabel → gr nodeLabel edgeLabel
filterEdges predicate graph = Graph.delEdges (fmap (\ (V2 n m, _) → (n, m)) $ filter (not . predicate) (labEdges' graph)) graph
