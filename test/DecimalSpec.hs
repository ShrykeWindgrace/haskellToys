module DecimalSpec (spec) where

import           Data.Either        (isLeft)
import           Helpers            (parseGen)
import           Parsers.Primitives (decimal)
import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec

parserHelper :: Integer -> Either (ParseError (Token String) Dec) Integer
parserHelper = parseGen decimal . show


tester :: Integer -> Bool
tester x
    | x < 0 = isLeft $ parserHelper x
    | otherwise = Right x == parserHelper x

spec :: Spec
spec = describe "decimal parser" $ it "should correctly parse positive integers" $
    property tester
