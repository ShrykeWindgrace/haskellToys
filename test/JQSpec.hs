{-# LANGUAGE RecordWildCards #-}
module JQSpec (spec) where


{-}
import           Control.Monad      (zipWithM_)
import           Data.Maybe
import           Helpers
import           Parsers.ImageLinks
import           Structures.Words
import           Test.Hspec


-}
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
import           Test.QuickCheck
import           Text.Megaparsec
import           Text.Megaparsec.String


spec :: Spec
spec = describe "placeholder" $ it "placeholder" $ parse testGrammar "" testLine `shouldBe` Right expectedResult

testLine :: String
testLine = "? вышел ОН из\nтумана\n№ ноль\n! (img w = 20px h =   40px moon.jpg) месяц\n!= moon"


pLine' = pLine <* char '\n'


testGrammar :: Parser Question
testGrammar = do
    string (parsingToken QText)
    skipSpaces
    l1 <- pLine'
    l2 <- pLine'
    modifierM <- questModifier
    char '\n'
    let modifier = fromJust modifierM
    string (parsingToken QAnswer)
    skipSpaces
    l3 <- pLine'
    string (parsingToken QNotEquiv)
    skipSpaces
    l4 <- pLine
    let fields = [QField QText [l1, l2] , QField QAnswer [l3] , QField QNotEquiv [l4]]
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
