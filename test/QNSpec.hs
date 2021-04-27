module QNSpec (spec) where

import           Constants.StringWorks (parsingToken)
import           Data.Either           (isLeft, rights)
import           Helpers               (parseGen, ParseResult)
import           Parsers.QN            (qHardReset, qSoftReset)
import           Structures.QNumber    (QModifier (..))
import           Test.Hspec            (Expectation, Spec, describe, it)
import           Test.QuickCheck       (property)
import Test.Hspec.Megaparsec


parserHelperHard :: Integer -> ParseResult QModifier
parserHelperHard n = parseGen qHardReset (forParse qm) where
    qm = Hard n

parserHelperSoft :: String -> ParseResult QModifier
parserHelperSoft str = parseGen qSoftReset (forParse qm) where
    qm = Soft str

forParse :: QModifier -> String
forParse qm = unwords $ [parsingToken, show] <*> [qm]


testerHard :: Integer -> Bool
testerHard x
    | x < 0 = isLeft $ parserHelperHard x
    | otherwise = [Hard x] == rights [parserHelperHard x]


testerSoft :: String -> Expectation -- for now test strings with one word
testerSoft str = parserHelperSoft str `shouldParse` Soft str

spec :: Spec
spec = do
    describe "qHardReset" $ it "should correctly parse hard reset" $
        property testerHard
    describe "qSoftReset" $ it "should correctly parse soft resets" $
        testerSoft "номер"
