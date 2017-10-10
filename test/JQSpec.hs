module JQSpec (spec) where

import           Structures.Lines       (Line (Line))
import           Structures.QNumber     (QModifier (Soft))
import           Structures.Quest       (QField (..), QFieldType (..), Question(..))
import           Structures.Words       (OneWord (..), ILink(ILink))

import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Text.Megaparsec        (parse)
import           Parsers.Question


spec :: Spec
spec = describe "placeholder" $ it "placeholder" $ parse parseQuest "" testLine `shouldBe` Right expectedResult

testLine :: String
testLine = "? вышел ОН из\nтумана\n№ ноль\n! (img w = 20px h =   40px moon.jpg) месяц\n!= moon"




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
