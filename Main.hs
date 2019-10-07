module Main where

import qualified Data.List as List

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import Planar
import qualified Layout
import Draw
import Instances

main :: IO ()
main = mainWith @(Diagram B) $ List.foldl1' (|||) graphs
  where
    graphs = fmap ((square 2.2 <>) . draw) $
        [ Layout.random kin248
        , Layout.circular kin248
        , Layout.random dag4
        , Layout.circular dag4
        ]
