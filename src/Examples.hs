module Examples where

import Protolude
import Data.Graph.Inductive (DynGraph, Gr, Node)
import Data.Graph.Inductive.Example
import Data.Bitraversable
import Instances

data AnyGraph = forall graph vertex edge.
  ( DynGraph graph, Bicontainer graph, Bitraversable graph
  , Eq vertex, Ord edge
  , forall vertex edge. (Eq vertex, Ord edge) => Eq (graph vertex edge)
  , IndexL graph ~ Int )
  => AnyGraph { graph :: graph vertex edge }

examples :: [(Text, AnyGraph)]
examples =
  [ ("a"      , AnyGraph { graph = a             })
  , ("b"      , AnyGraph { graph = b             })
  , ("clr486" , AnyGraph { graph = clr486        })
  , ("clr528" , AnyGraph { graph = clr528        })
  , ("d3"     , AnyGraph { graph = d3            })
  , ("e"      , AnyGraph { graph = e             })
  , ("loop"   , AnyGraph { graph = loop          })
  , ("vor"    , AnyGraph { graph = vor           })
  , ("ab"     , AnyGraph { graph = ab            })
  , ("c"      , AnyGraph { graph = c             })
  , ("clr489" , AnyGraph { graph = clr489        })
  , ("clr595" , AnyGraph { graph = clr595        })
  , ("dag3"   , AnyGraph { graph = dag3          })
  , ("e3"     , AnyGraph { graph = e3            })
  , ("gr1"    , AnyGraph { graph = gr1           })
  , ("cyc3"   , AnyGraph { graph = cyc3          })
  , ("kin248" , AnyGraph { graph = kin248        })
  , ("star"   , AnyGraph { graph = star @Gr 13   })
  , ("abb"    , AnyGraph { graph = abb           })
  , ("clr479" , AnyGraph { graph = clr479        })
  , ("clr508" , AnyGraph { graph = clr508        })
  , ("d1"     , AnyGraph { graph = d1            })
  , ("dag4"   , AnyGraph { graph = dag4          })
  , ("g3"     , AnyGraph { graph = g3            })
  , ("g3b"    , AnyGraph { graph = g3b           })
  , ("ucycle" , AnyGraph { graph = ucycle @Gr 13 })
  ]
