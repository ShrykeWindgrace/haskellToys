module Structures.Quest where

import Structures.QNumber
import Structures.Lines
import Render.StringWorks


data Question = Question {
    modifier :: QModifierM,
    text :: [Line],
    answer :: [Line]
} deriving (Show, Eq)


data Tour = Tour {
    quests :: [Question],
    comment :: Maybe Comment
    } deriving (Show, Eq)


newtype Comment = Comment { unComment :: String } deriving (Eq, Show)


instance ShowNatural Comment where
    showNatural = unComment


instance HasToken Question where
    tokenOf = const "?"

instance HasToken Comment where
    tokenOf = const "#"

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

instance HasToken QFieldType where
    tokenOf = show



instance Read QFieldType where
    readsPrec _ str = [(charToFT str, "")] where
        charToFT :: String -> QFieldType
        charToFT "?" = QText -- "Вопрос"
        charToFT "=" = QEquiv -- "Зачёт"
        charToFT "@" = QAuthor -- "Автор(ы)"
        charToFT "^" = QSource -- "Источник(и)"
        charToFT "!" = QAnswer -- "Ответ"
        charToFT "/" = QComment -- "Комментарии"
        charToFT "!="= QNotEquiv -- "незачёт"
        charToFT _   = error "illegal question field type identifier"
        