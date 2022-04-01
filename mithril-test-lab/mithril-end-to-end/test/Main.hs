module Main where

import qualified Spec
import Test.Hspec.Runner
import Test.Hydra.Prelude (dualFormatter)

main :: IO ()
main = hspecWith defaultConfig {configFormat = Just (dualFormatter "mithril-end-to-end")} Spec.spec
