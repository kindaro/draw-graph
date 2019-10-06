module Draw where

import Diagrams.Prelude (Diagram)
import Data.Graph.Inductive (DynGraph)

randomDraw :: DynGraph gr => gr a b -> Diagram backEnd
randomDraw = _u
