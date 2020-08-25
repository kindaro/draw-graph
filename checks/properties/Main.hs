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
  ]
