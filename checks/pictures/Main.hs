{-# language NoOverloadedStrings #-}

module Main where

import Protolude
import Prelude (String)
import qualified Data.ByteString.Lazy as LazyBytes
import Data.Text (Text)
import qualified Data.Text as Text
import Diagrams.Backend.Rasterific
import Codec.Picture
import Text.Cute.String
import Test.Tasty
import Test.Tasty.Golden
import Examples
import Draw.Examples
import Diagrams

main ∷ IO ( )
main = defaultMain $ testGroup "Standard examples." $ fmap verifyExample examples

verifyExample ∷ (Text, AnyGraph) → TestTree
verifyExample x@(name, _) = goldenVsStringDiff
  (Text.unpack name)
  compareImages
  (("checks/pictures/"…".png") (Text.unpack name))
  ((return . renderToPngBytes . renderOne) x)

compareImages ∷ FilePath → FilePath → [String]
compareImages expected actual = ["sh", "-c", commandLine]
  where
    commandLine = ("if ! cmp '"…"' '"…"'; then \
                   \compare '"…"' '"…"' png:- | \
                   \montage -geometry +4+4 '"…"' - '"…"' png:- > '"…".diff.png'; \
                   \echo 'See "…".diff.png for difference.'; false; fi")
      actual expected actual expected actual expected expected expected

renderToPngBytes ∷ Diagram Rasterific → LazyBytes.ByteString
renderToPngBytes = encodePng . renderDia Rasterific (RasterificOptions (mkWidth 250))
