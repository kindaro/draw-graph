module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified MyLib (someFunc)

main :: IO ()
main = mainWith (circle 1 :: Diagram B)
