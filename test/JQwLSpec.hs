module JQwLSpec (spec) where

import           Constants.StringWorks (parsingToken)
import           Structures.Composers  (strToLine)
import           Structures.Lines      (Line (..), ListLines (..))
import           Structures.Quest      (QField (..), QFieldType (..),
                                        Question (..), Tour (..))
import           Structures.Words      (OneWord (..))

import           Parsers.Question      (parseQuest, parseTour)
import           Test.Hspec            (Spec, describe, it)
import           Test.Hspec.Megaparsec (shouldParse)
import           Text.Megaparsec       (parse)


spec :: Spec
spec = do
    describe "question parser" $ it "parse question with lists" $ parse parseQuest "" testLine `shouldParse` expectedResult
    describe "tour parser" $ it "parse a tour with one question" $ parse parseTour "" (tourPrefix ++ testLine) `shouldParse` expectedTourResult

testLine :: String
testLine = "? Блиц.\nПо мнению Анатолия Коломейского, если бы персонажи его миниатюр могли говорить, они сказали бы следующее:\n- «Мне все говорят: „Дура, дура!..“. А я не обижаюсь. Потому что я — ОНА». Назовите ЕЁ.\n- «Мне завидуют, мол, медовый месяц — круглый год. Но это нормально, потому что я — ОНА». Назовите ЕЁ.\n- «Популярность утомляет. Открытие кинофестиваля в Каннах — я в кадре! Приём у президента — я в зале! Встреча делегации — без меня ни шагу. Потому что я — ОНА». Назовите ЕЁ двумя словами.\n!\n- пуля.\n- пчела.\n- ковровая дорожка.\n= 3. красная дорожка.\n^ «Вокруг смеха», 2010, N 3.\n@ Ольга Неумывакина (Харьков)"

tourPrefix :: String
tourPrefix = parsingToken expectedTourResult ++ "\n"



expectedResult :: Question
expectedResult = Question {
    modifier = Nothing,
    fields = [
      QField QText [
                    Line [RegWord "Блиц."],
                    strToLine "По мнению Анатолия Коломейского, если бы персонажи его миниатюр могли говорить, они сказали бы следующее:",
                    ListLinesStr $ ListLines [
                        strToLine "«Мне все говорят: „Дура, дура!..“. А я не обижаюсь. Потому что я — ОНА». Назовите ЕЁ.",
                        strToLine "«Мне завидуют, мол, медовый месяц — круглый год. Но это нормально, потому что я — ОНА». Назовите ЕЁ.",
                        strToLine "«Популярность утомляет. Открытие кинофестиваля в Каннах — я в кадре! Приём у президента — я в зале! Встреча делегации — без меня ни шагу. Потому что я — ОНА». Назовите ЕЁ двумя словами."
                      ]
                    ],
              QField QAnswer [
                ListLinesStr $ ListLines [Line [RegWord "пуля."], Line [RegWord "пчела."], Line [RegWord "ковровая", RegWord "дорожка." ]]
              ],

              QField QEquiv [Line [RegWord "3.", RegWord "красная", RegWord "дорожка." ]],
              QField QSource [strToLine "«Вокруг смеха», 2010, N 3."],
              QField QAuthor [strToLine "Ольга Неумывакина (Харьков)"]
              ]
    }


expectedTourResult :: Tour
expectedTourResult = Tour [expectedResult] Nothing Nothing
