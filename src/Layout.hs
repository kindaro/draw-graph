module Layout where

import Data.Bitraversable
import Control.Monad.State
import Linear.V2
import Data.Graph.Inductive (DynGraph)
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import System.Random

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
