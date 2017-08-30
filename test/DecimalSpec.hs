module DecimalSpec (spec) where

import           Data.Either     (isLeft)
import           Helpers         (parseGen)
import           Parsers.Inline          (decimal)
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec

parserHelper :: Integer -> Either ParseError Integer
parserHelper = parseGen decimal . show


tester :: Integer -> Bool
tester x
    | x < 0 = isLeft $ parserHelper x
    | otherwise = Right x == parserHelper x

spec = describe "decimal parser" $ it "should correctly parse positive integers" $
    property $ \x -> tester (x:: Integer)
