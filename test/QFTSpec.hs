module QFTSpec (spec) where

import           Constants.StringWorks (parsingToken)
import           Data.Foldable         (traverse_)
import           Helpers               (ParseResult, parseGen)
import           Parsers.Field         (fieldType)
import           Structures.Quest      (QFieldType (..), allQFTs)
import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.Megaparsec (shouldFailOn, shouldParse,
                                        shouldSucceedOn)


parserHelper :: QFieldType -> ParseResult QFieldType
parserHelper ft = parseGen (fieldType ft) (parsingToken ft <> " ")

ownParseTest :: Spec
ownParseTest = describe "question field type parser should correctly parse its own strings" $ traverse_ (\x -> it (show x)  (runner x) ) allQFTs where
    runner qft = parserHelper qft `shouldParse` qft

foreignParseTest :: Spec
foreignParseTest = describe "question field type parser should correctly parse only its own strings" $ traverse_ (\(a,b,r) -> it (unwords ["testing", show a, "<->", show b]) r)
    [ (l, r, cmp (runner l)  (parsingToken r <> " ")) | l <- allQFTs , r <- allQFTs , let cmp = if l == r then shouldSucceedOn else shouldFailOn ] where
        runner :: QFieldType -> String -> ParseResult QFieldType
        runner qft = parseGen (fieldType qft)

spec :: Spec
spec = do
    ownParseTest
    foreignParseTest
