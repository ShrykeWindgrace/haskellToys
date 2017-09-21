module Structures.Quest where

import Structures.QNumber
import Structures.Lines
import Render.StringWorks


data Question = Question {
    modifier :: QModifierM,
    text :: [Line],
    answer :: [Line]
} deriving (Show)

data Tour = Tour [Question] deriving (Show)

data QFieldType = QText | QAnswer | QEquiv | QAuthor | QComment | QNotEquiv | QSource deriving (Eq)

instance Show QFieldType where
    show QText = "?"
    show QEquiv = "="
    show QAuthor = "@"
    show QSource = "^"
    show QAnswer = "!"
    show QComment = "/"
    show QNotEquiv = "!="

instance ShowNatural QFieldType where
    showNatural = tokenToString . show 

charToFT :: String -> QFieldType
charToFT "?" = QText -- "Вопрос"
charToFT "=" = QEquiv -- "Зачёт"
charToFT "@" = QAuthor -- "Автор(ы)"
charToFT "^" = QSource -- "Источник(и)"
charToFT "!" = QAnswer -- "Ответ"
charToFT "/" = QComment -- "Комментарии"
charToFT "!="= QNotEquiv -- "незачёт"
charToFT _   = error "illegal question field type identifier"
