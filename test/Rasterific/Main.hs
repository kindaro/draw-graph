module Main where

import Data.Graph.Inductive.Arbitrary ()
import Data.Graph.Inductive.PatriciaTree
import Diagrams.Backend.Rasterific
import Control.DeepSeq
import Diagrams.Prelude (renderDia, mkWidth)
import Test.Tasty
import Test.Tasty.QuickCheck

import Draw
import Layout

main :: IO ()
main = defaultMain $ testGroup ""
    [ testProperty "" \g -> within (10 ^ (6 :: Int))
        let graph = (g :: Gr () ())
            options = RasterificOptions (mkWidth 100)
            render = renderDia Rasterific options . draw . circular
        in counterexample (show $ circular graph)
            $ render graph `deepseq` True
    ]
