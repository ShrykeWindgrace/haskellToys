module HogHeaderTest (hogCheck) where

import           Constants.StringWorks (parsingToken)
import           Data.Either           (isRight)
import           Hedgehog              (Property, assert, check, forAll,
                                        property)
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Helpers               (ParseResult, parseGen)
import           Parsers.Header        (parseEditor)
import           Structures.Header     (HeaderItem, HeaderItemType (Editor))


parserHelper :: String -> ParseResult HeaderItem
parserHelper n = parseGen parseEditor $ unwords [parsingToken Editor, n]

hogCheck :: IO ()
hogCheck = () <$ check parseHeader

parseHeader :: Property
parseHeader = property $ do
    xs <- forAll $ Gen.string  (Range.linear 1 100) Gen.alphaNum
    assert $ isRight $ parserHelper xs  -- ::  ()
    -- return ()
