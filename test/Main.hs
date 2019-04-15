module Main where

import qualified Spec
import           Test.Hspec.Formatters (progress)
import           Test.Hspec.Runner (configFormatter, hspecWith, defaultConfig)

import qualified HogDecimalTest as HDT
import qualified HogHeaderTest  as HHT

main :: IO ()
main = do
    hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
    sequence_ [HDT.hogCheck, HHT.hogCheck]

