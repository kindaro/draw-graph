module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Data.Graph.Inductive.Example
import Data.Graph.Inductive.PatriciaTree

import Planar
import Layout
import Draw
import Instances

main :: IO ()
main = mainWith @(Diagram B) $ draw @Gr @B . randomLayout $ kin248
