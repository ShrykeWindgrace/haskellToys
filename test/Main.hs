module Main where

import qualified Spec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

import           HogDecimalTest        (hogCheck)

main :: IO ()
main = do
    hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
    hogCheck
