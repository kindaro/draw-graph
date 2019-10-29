module Examples where

import Protolude
import Data.Graph.Inductive (DynGraph, Gr, Node)
import Data.Graph.Inductive.Example
import Data.Bitraversable
import Instances

data AnyGraph = forall gr a b.
              (DynGraph gr, Bicontainer gr, Bitraversable gr, Eq (gr a b), IndexL gr ~ Int)
              => AnyGraph { graph :: gr a b, name :: Text }

examples :: [AnyGraph]
examples =
  [ AnyGraph { graph = a             , name = "a"      }
  , AnyGraph { graph = b             , name = "b"      }
  , AnyGraph { graph = clr486        , name = "clr486" }
  , AnyGraph { graph = clr528        , name = "clr528" }
  , AnyGraph { graph = d3            , name = "d3"     }
  , AnyGraph { graph = e             , name = "e"      }
  , AnyGraph { graph = loop          , name = "loop"   }
  , AnyGraph { graph = vor           , name = "vor"    }
  , AnyGraph { graph = ab            , name = "ab"     }
  , AnyGraph { graph = c             , name = "c"      }
  , AnyGraph { graph = clr489        , name = "clr489" }
  , AnyGraph { graph = clr595        , name = "clr595" }
  , AnyGraph { graph = dag3          , name = "dag3"   }
  , AnyGraph { graph = e3            , name = "e3"     }
  , AnyGraph { graph = gr1           , name = "gr1"    }
  , AnyGraph { graph = cyc3          , name = "cyc3"   }
  , AnyGraph { graph = kin248        , name = "kin248" }
  , AnyGraph { graph = star @Gr 13   , name = "star"   }
  , AnyGraph { graph = abb           , name = "abb"    }
  , AnyGraph { graph = clr479        , name = "clr479" }
  , AnyGraph { graph = clr508        , name = "clr508" }
  , AnyGraph { graph = d1            , name = "d1"     }
  , AnyGraph { graph = dag4          , name = "dag4"   }
  , AnyGraph { graph = g3            , name = "g3"     }
  , AnyGraph { graph = g3b           , name = "g3b"    }
  , AnyGraph { graph = ucycle @Gr 13 , name = "ucycle" }
  ]
