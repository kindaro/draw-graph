module Main where

import Protolude
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified Layout
import Draw.Examples
import Analyze
import Examples

main :: IO ()
main = mainWith @(Diagram B)
     $ vcat . fmap renderAutumn $ examples

renderAutumn :: (Text, AnyGraph) -> Diagram B
renderAutumn (name, AnyGraph{..})
  = graph
  & Layout.circular
  & autumn
  & fmap (\ x -> decorateLaidOutGraph (name, x))
  & hcat
