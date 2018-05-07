module Main where

import qualified Spec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

import qualified HogDecimalTest as HDT
import qualified HogHeaderTest  as HHT

main :: IO ()
main = do
    hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
    HDT.hogCheck
    HHT.hogCheck

