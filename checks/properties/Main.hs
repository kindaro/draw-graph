module Main where

import Protolude
import Test.Invariant
import Data.Graph.Inductive
import Data.Graph.Inductive.Arbitrary ( )
import Test.Tasty
import Test.Tasty.QuickCheck
import Draw

main ∷ IO ( )
main = defaultMain $ testGroup "Properties."
  [ testProperty "Edges can be contracted and expanded" $ (expandEdges @Gr @Int @Int) `inverts` compactifyEdges
  , testProperty "Unordered pair pattern is correct." $ u2ToV2 . uncurry (U2 @Int) <=> u2ToV2 . uncurry (flip U2)
  ]
