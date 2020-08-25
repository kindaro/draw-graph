{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Protolude
import Test.Invariant
import Data.Graph.Inductive
import Data.Graph.Inductive.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Prelude.Unicode ((∧))
import Diagrams (V2 (V2))
import Draw

instance (Arbitrary a, Ord a) => Arbitrary (MultiSet a) where
  arbitrary = MultiSet.fromList <$> arbitrary
  shrink = fmap MultiSet.fromList . shrink . MultiSet.toList

-- | Surely an expandable graph.
newtype G = G_ {unG_ ∷ NoMultipleEdges Gr (Char, MultiSet Char) (MultiSet Char, MultiSet Char)} deriving (Eq, Show)

unG :: G → Gr (Char, MultiSet Char) (MultiSet Char, MultiSet Char)
unG = nmeGraph . unG_

pattern G ∷ Gr (Char, MultiSet Char) (MultiSet Char, MultiSet Char) → G
pattern G x ← G_ (NME x)
  where G = G_ . NME . filterEdges (\ ((V2 n m), (xs, ys)) → n < m ∧ not (null xs) ∧ not (null ys))

instance Arbitrary G where
  arbitrary = G . unG . G_ <$> arbitrary
  shrink = fmap G . shrink . unG

main ∷ IO ( )
main = defaultMain $ testGroup "Properties."
  [ testProperty "Edges can be contracted and expanded" $ (expandEdges @Gr @Int @Int) `inverts` compactifyEdges
  , testProperty "Edges can be expanded and contracted" $ (G_ . NME . compactifyEdges) `inverts` (expandEdges . unG)
  , testProperty "Unordered pair pattern is correct." $ u2ToV2 . uncurry (U2 @Int) <=> u2ToV2 . uncurry (flip U2)
  ]
