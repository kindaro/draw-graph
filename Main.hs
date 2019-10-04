module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified MyLib (someFunc)

main :: IO ()
main = mainWith @(Diagram B) $ square 1 ||| square 1
