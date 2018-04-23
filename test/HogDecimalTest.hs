module HogDecimalTest (hogCheck) where

import           Hedgehog           (Property, check, forAll, property, (===))
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import           Helpers            (ParseResult, parseGen)
import           Parsers.Primitives (decimal)

parserHelper :: Integer -> ParseResult Integer
parserHelper = parseGen decimal . show

parseInt :: Property
parseInt = property $ do
    xs <- forAll $ Gen.integral (Range.linear 0 1000)
    Right xs === parserHelper xs

hogCheck :: IO ()
hogCheck = () <$ check parseInt
