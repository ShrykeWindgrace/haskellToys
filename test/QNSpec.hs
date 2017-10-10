module QNSpec (spec) where

import           Constants.StringWorks (parsingToken)
import           Data.Either           (isLeft, isRight, fromRight)
import           Helpers               (parseGen, ParseResult)
import           Parsers.QN            (qHardReset, qSoftReset)
import           Structures.QNumber    (QModifier (..))
import           Test.Hspec            (Expectation, Spec, describe, it,
                                        shouldBe)
import           Test.QuickCheck       (property)


parserHelperHard :: Integer -> ParseResult QModifier
parserHelperHard n = parseGen qHardReset (parsingToken Hard{} ++ " " ++ show n)

parserHelperSoft :: String -> ParseResult QModifier
parserHelperSoft str = parseGen qSoftReset (parsingToken Soft{} ++ " " ++ str)

testerHard :: Integer -> Bool
testerHard x
    | x < 0 = isLeft $ parserHelperHard x
    | otherwise = isRight result && Hard x == fromRight' result where
        result = parserHelperHard x


testerSoft :: String -> Expectation -- for now test strings with one word
testerSoft str = Soft str `shouldBe` fromRight' (parserHelperSoft str)

spec :: Spec
spec = do
    describe "qHardReset" $ it "should correctly parse hard reset" $
        property testerHard
    describe "qSoftReset" $ it "should correctly parse soft resets" $
        testerSoft "номер"


{- unpack values that are guaranteed to be Right{} -}
fromRight' :: Either a b -> b
fromRight' = fromRight undefined