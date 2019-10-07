module Main where

import qualified Data.List as List
import Data.Bifunctor
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Data.Graph.Inductive.Example

import qualified Layout
import Draw

main :: IO ()
main = mainWith @(Diagram B)
     $ (square 2.2 <>) . draw . Layout.circular . bimap (const 1) id
     $ clr489
