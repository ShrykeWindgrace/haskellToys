module JQwLSpec (spec) where

import           Structures.Composers (strToLine)
import           Structures.Lines     (Line (..), ListLines (..))
import           Structures.QNumber   (QModifier (Hard))
import           Structures.Quest     (QField (..), QFieldType (..),
                                       Question (..))
import           Structures.Words     (OneWord (..))

import           Parsers.Question
import           Test.Hspec           (Spec, describe, it, shouldBe)
import           Text.Megaparsec      (parse)


spec :: Spec
spec = describe "placeholder" $ it "placeholder" $ parse parseQuest "" testLine `shouldBe` Right expectedResult

testLine :: String
testLine = "? Блиц.\nПо мнению Анатолия Коломейского, если бы персонажи его миниатюр могли говорить, они сказали бы следующее:\n- «Мне все говорят: „Дура, дура!..“. А я не обижаюсь. Потому что я — ОНА». Назовите ЕЁ.\n- «Мне завидуют, мол, медовый месяц — круглый год. Но это нормально, потому что я — ОНА». Назовите ЕЁ.\n- «Популярность утомляет. Открытие кинофестиваля в Каннах — я в кадре! Приём у президента — я в зале! Встреча делегации — без меня ни шагу. Потому что я — ОНА». Назовите ЕЁ двумя словами.\n!\n- пуля.\n- пчела.\n- ковровая дорожка.\n= 3. красная дорожка.\n^ «Вокруг смеха», 2010, N 3.\n@ Ольга Неумывакина (Харьков)"




expectedResult :: Question
expectedResult = Question {
    modifier = Hard 0,
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
