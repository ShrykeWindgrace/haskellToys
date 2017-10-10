{-# LANGUAGE RecordWildCards #-}
module JQSpec (spec) where

import           Data.Maybe             (fromJust)
import           Parsers.Lines          (parseQFall)
import           Parsers.QN             (questModifier)
import           Structures.Lines       (Line (Line))
import           Structures.QNumber     (QModifier (Soft))
import           Structures.Quest       (QField (..), QFieldType (..), Question(..))
import           Structures.Words       (OneWord (..), ILink(ILink))

import           Test.Hspec             (Spec, describe, it, shouldBe)
import           Text.Megaparsec        (many, parse, some)
import           Text.Megaparsec.String (Parser)


spec :: Spec
spec = describe "placeholder" $ it "placeholder" $ parse testGrammar "" testLine `shouldBe` Right expectedResult

testLine :: String
testLine = "? вышел ОН из\nтумана\n№ ноль\n! (img w = 20px h =   40px moon.jpg) месяц\n!= moon"



testGrammar :: Parser Question
testGrammar = do
    pre <- many parseQFall -- QText
    modifierM <- questModifier
    let modifier = fromJust modifierM
    post <- some parseQFall -- answer must be there
    let fields = pre ++ post
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
