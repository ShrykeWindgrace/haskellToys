module Structures.Quest where

import Structures.QNumber
import Structures.Lines


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
    show QText = "?"
    show QAuthor = "@"
    show QSource = "^"
    show QAnswer = "!"
    show QComment = "/"
    show QNotEquiv = "!="

charToFT :: String -> QFieldType
charToFT "?" = QText -- "Вопрос"
charToFT "=" = QEquiv -- "Зачёт"
charToFT "@" = QAuthor -- "Автор(ы)"
charToFT "^" = QSource -- "Источник(и)"
charToFT "!" = QAnswer -- "Ответ"
charToFT "/" = QComment -- "Комментарии"
charToFT "!="= QNotEquiv -- "незачёт"
charToFT _   = error "illegal question field type identifier"
