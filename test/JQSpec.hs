module JQSpec (spec) where

import           Structures.Composers (strToLine)
import           Structures.Lines     (Line (Line))
import           Structures.QNumber   (QModifier (Soft))
import           Structures.Quest     (QField (..), QFieldType (..),
                                       Question (..))
import           Structures.Words     (ILink (ILink), OneWord (..))

import           Parsers.Question
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Text.Megaparsec      (parse)


spec :: Spec
spec = describe "placeholder" $ it "placeholder" $ parse parseQuest "" testLine `shouldBe` Right expectedResult

testLine :: String
testLine = "? вышел ОН из\nтумана\n№ ноль\n! (img w = 20px h =   40px moon.jpg) месяц\n!= moon"




expectedResult :: Question
expectedResult = Question {
    modifier = Soft "ноль",
    fields = [QField QText [ strToLine "вышел ОН из", Line [RegWord "тумана"] ],
              QField QAnswer [
                    Line[ILinkStr $ ILink "moon.jpg" (Just 20) (Just 40), RegWord "месяц"]],
              QField QNotEquiv [Line [RegWord "moon"]]]
    }
