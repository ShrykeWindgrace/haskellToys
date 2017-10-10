module DecimalSpec (spec) where

import           Data.Either        (isLeft)
import           Helpers            (parseGen, ParseResult)
import           Parsers.Primitives (decimal)
import           Test.Hspec         (Spec, describe, it)
import           Test.QuickCheck    (property)
import           Text.Megaparsec    (Dec, ParseError, Token)

parserHelper :: Integer -> ParseResult Integer
parserHelper = parseGen decimal . show


tester :: Integer -> Bool
tester x
    | x < 0 = isLeft $ parserHelper x
    | otherwise = Right x == parserHelper x

spec :: Spec
spec = describe "decimal parser" $ it "should correctly parse positive integers" $
    property tester
