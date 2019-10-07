module Layout where

import Data.Bitraversable
import Control.Monad.State
import Linear.V2
import Data.Graph.Inductive (DynGraph)
import qualified Data.Graph.Inductive as Graph
import Data.Stream (Stream)
import qualified Data.Stream as Stream
import System.Random

random :: (DynGraph gr, Bitraversable gr) => gr a b -> gr (V2 Double, a) b
random = bilabelWith (flip (,)) const randomPoints (Stream.repeat ())
  where
    randomCoordinates = Stream.fromList $ randomRs (-1, 1) $ mkStdGen 0

    streamToTuples (Stream.Cons x (Stream.Cons y upstream)) =
                                                    Stream.Cons (x, y) (streamToTuples upstream)

    randomPoints :: Stream (V2 Double)
    randomPoints = let f = fmap (uncurry V2) . streamToTuples in f randomCoordinates

circular :: (DynGraph gr, Bitraversable gr) => gr a b -> gr (V2 Double, a) b
circular gr =
  let n = fromIntegral (Graph.noNodes gr)
      coords =
        let alpha = (pi * 2 / n)
        in Stream.prefix [ 0.8 * V2 (sin (alpha * i)) (cos (alpha * i)) | i <- [1.. n] ]
            $ error "circular: Impossible error: not enough coordinates to label all points."
  in bilabelWith (flip (,)) const coords (Stream.repeat ()) gr

label :: Traversable t => Stream index -> t a -> t (index, a)
label = labelWith (flip (,))

labelWith :: Traversable t => (a -> index -> b) -> Stream index -> t a -> t b
labelWith f xs = flip evalState xs . traverse (labelOneWith f)

bilabelWith :: Bitraversable t => (l -> indexl -> l') -> (r -> indexr -> r')
                               -> Stream indexl       -> Stream indexr       -> t l r -> t l' r'
bilabelWith f g xs ys =
  let labelLeft  = fmap (withStateLens fst (\(x, y) x' -> (x', y))) (labelOneWith f)
      labelRight = fmap (withStateLens snd (\(x, y) y' -> (x, y'))) (labelOneWith g)
  in flip evalState (xs, ys) . bitraverse labelLeft labelRight

labelOne :: a -> State (Stream index) (index, a)
labelOne = labelOneWith (flip (,))

labelOneWith :: (a -> index -> b) -> a -> State (Stream index) b
labelOneWith f u = do
    (Stream.Cons x xs) <- get
    put xs
    return (f u x)

withStateLens :: (s -> t) -> (s -> t -> s) -> State t a -> State s a
withStateLens thrust merge u = do
    s <- get
    let t = thrust s
    let (r, t') = runState u t
    put (merge s t')
    return r
