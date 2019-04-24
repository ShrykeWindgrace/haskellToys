module HogHeaderTest (hogCheck) where

import           Constants.StringWorks (parsingToken)
import           Data.Either           (isRight)
import           Hedgehog              (Property, assert, check, forAll,
                                        property)
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Helpers               (ParseResult, parseGen)
import           Parsers.Header        (parseHeaderGen)
import           Structures.Header     (HeaderItem, HeaderItemType (..))


parserHelper :: HeaderItemType -> String -> ParseResult HeaderItem
parserHelper t n = parseGen (parseHeaderGen t) $ unwords [parsingToken t, n]

hogCheck :: IO Bool -- ()
hogCheck = {-  -}check parseHeader

parseHeader :: Property
parseHeader = property $ do
    hs <- forAll $ Gen.enum Editor TDate
    xs <- forAll $ Gen.string  (Range.linear 1 100) Gen.alphaNum
    assert $ isRight $ parserHelper hs xs  -- ::  ()
    -- return ()
