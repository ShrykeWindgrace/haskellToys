module QNSpec (spec) where

import           Constants.StringWorks
import           Data.Either           (isLeft)
import           Helpers               (parseGen)
import           Parsers.QN
import           Structures.QNumber
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec

parserHelper :: Integer -> Either ParseError QModifier
parserHelper n = parseGen qHardReset (parsingToken (Hard n) ++ " " ++ show n)

tester :: Integer -> Bool
tester x
    | x < 0 = isLeft $ parserHelper x
    | otherwise = Right (Hard x) == parserHelper x

spec :: Spec
spec = describe "qHardReset" $ it "should correctly parse hard reset" $
    property tester
