{-# LANGUAGE RecordWildCards #-}
module JQSpec (spec) where

import           Constants.StringWorks
import           Data.Maybe             (fromJust)
import           Parsers.InlineSpace
import           Parsers.Lines
import           Parsers.QN
import           Structures.Lines
import           Structures.QNumber
import           Structures.Quest
import           Structures.Words

import           Test.Hspec
import           Text.Megaparsec        (Dec, ParseError, Token, parse, string)
import           Text.Megaparsec.String


spec :: Spec
spec = do
    describe "placeholder" $ it "placeholder" $ parse testGrammar "" testLine `shouldBe` Right expectedResult
    describe "placeholder" $ it "placeholder" $ parse testGrammar2 "" testLine `shouldBe` Right expectedResult

testLine :: String
testLine = "? вышел ОН из\nтумана\n№ ноль\n! (img w = 20px h =   40px moon.jpg) месяц\n!= moon"


testGrammar :: Parser Question
testGrammar = do
    _<-string (parsingToken QText)
    skipSpaces
    l1 <- pLineExternal
    l2 <- pLineExternal
    modifierM <- questModifier
    let modifier = fromJust modifierM
    _<-string (parsingToken QAnswer)
    skipSpaces
    l3 <- pLineExternal
    _<-string (parsingToken QNotEquiv)
    skipSpaces
    l4 <- pLineExternal
    let fields = [QField QText [l1, l2] , QField QAnswer [l3] , QField QNotEquiv [l4]]
    return Question{..}


testGrammar2 :: Parser Question
testGrammar2 = do
    f1 <- parseQF QText
    modifierM <- questModifier
    let modifier = fromJust modifierM
    f2 <- parseQF QAnswer
    f3 <- parseQF QNotEquiv
    let fields = [f1, f2, f3]
    return Question{..}


expectedResult :: Question
expectedResult = Question {
    modifier = Soft "ноль",
    fields = [QField QText [
                    Line [RegWord "вышел", RegWord "ОН", RegWord "из"],
                    Line [RegWord "тумана"]
                ],
              QField QAnswer [
                    Line[ILinkStr $ ILink "moon.jpg" (Just 20) (Just 40), RegWord "месяц"]],
              QField QNotEquiv [Line [RegWord "moon"]]]
    }
