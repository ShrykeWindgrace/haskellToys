module QNSpec (spec) where

import           Constants.StringWorks (parsingToken)
import           Data.Either           (isLeft)
import           Helpers               (parseGen)
import           Parsers.QN            (qHardReset, qSoftReset)
import           Structures.QNumber    (QModifier (..))
import           Test.Hspec            (Expectation, Spec, describe, it,
                                        shouldBe)
import           Test.QuickCheck       (property)
import           Text.Megaparsec       (Dec, ParseError, Token)

parserHelperHard :: Integer -> Either (ParseError (Token String) Dec) QModifier
parserHelperHard n = parseGen qHardReset (parsingToken Hard{} ++ " " ++ show n)

parserHelperSoft :: String -> Either (ParseError (Token String) Dec) QModifier
parserHelperSoft str = parseGen qSoftReset (parsingToken Soft{} ++ " " ++ str)

testerHard :: Integer -> Bool
testerHard x
    | x < 0 = isLeft $ parserHelperHard x
    | otherwise = Right (Hard x) == parserHelperHard x


testerSoft :: String -> Expectation -- for now test strings with one word
testerSoft str = Right (Soft str) `shouldBe` parserHelperSoft str

spec :: Spec
spec = do
    describe "qHardReset" $ it "should correctly parse hard reset" $
        property testerHard
    describe "qSoftReset" $ it "should correctly parse soft resets" $
        testerSoft "номер"
