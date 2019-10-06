module Layout where

import Data.Bifoldable
import Data.Bitraversable
import Control.Monad.State

import Linear.V2

import Data.Graph.Inductive (Graph, DynGraph, Node, Context, Decomp, Gr)
import qualified Data.Graph.Inductive as Graph

import Data.Stream (Stream)
import qualified Data.Stream as Stream

import System.Random

import Control.Comonad.Store

insert :: DynGraph gr => Context a b -> gr a b -> gr a b
insert = (Graph.&)

layout :: Graph gr => gr a b -> gr (V2 Double, a) b
layout = undefined

randomLayout :: (DynGraph gr, Bitraversable gr) => gr a b -> gr (V2 Double, a) b
randomLayout = flip evalState randomPoints
            . bitraverse labelOne pure
  where
    randomCoordinates = Stream.fromList $ randomRs (0, 1) $ mkStdGen 0

    streamToTuples (Stream.Cons x (Stream.Cons y upstream)) = Stream.Cons (x, y) (streamToTuples upstream)

    randomPoints :: Stream (V2 Double)
    randomPoints = let f = fmap (uncurry V2) . streamToTuples in f randomCoordinates

    labelOne :: a -> State (Stream (V2 Double)) (V2 Double, a)
    labelOne x = do
        (Stream.Cons label labels) <- get
        put labels
        return (label, x)


pattern Anywhere :: DynGraph gr => Graph.Context a b -> gr a b -> gr a b
pattern Anywhere context remainingGraph <- (Graph.matchAny -> (context, remainingGraph))
    where Anywhere context remainingGraph = insert context remainingGraph

pattern Empty :: Graph gr => gr a b
pattern Empty <- (Graph.isEmpty -> True)
    where Empty = Graph.empty

{-# complete Anywhere, Empty :: Gr #-}

instance Bifoldable Gr where
    bifoldr _ _ zero Empty = zero
    bifoldr mergeVertex mergeEdge zero (Anywhere (edgesIn, _, label, edgesOut) r) = 
      let mergeAdj adj zero' = foldr (mergeEdge . fst) zero' adj
          f = mergeAdj edgesOut . mergeAdj edgesIn . mergeVertex label
      in bifoldr mergeVertex mergeEdge (f zero) r

instance Bitraversable Gr where
    bitraverse _ _ Empty = pure Empty
    bitraverse effectVertex effectEdge (Anywhere (edgesIn, identifier, label, edgesOut) r) = do
        label' <- effectVertex label
        edgesIn' <- traverse (bitraverse effectEdge id . fmap pure) edgesIn
        edgesOut' <- traverse (bitraverse effectEdge id . fmap pure) edgesOut
        r' <- bitraverse effectVertex effectEdge r
        return $ insert (edgesIn', identifier, label', edgesOut') r'

storeGraph :: (Graph gr) => gr a b -> Node -> Store Node (Decomp gr a b)
storeGraph graph node = let f identifier = Graph.match identifier graph
                        in store f node
