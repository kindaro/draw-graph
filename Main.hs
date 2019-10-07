module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import Planar
import qualified Layout
import Draw
import Instances

main :: IO ()
main = mainWith @(Diagram B) $ (square 1 <> random) ||| (square 1 <> circular)
  where
    random   = draw @Gr @B . Layout.random   $ kin248
    circular = draw @Gr @B . Layout.circular $ kin248
