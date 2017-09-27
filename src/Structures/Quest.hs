module Structures.Quest where

import Structures.QNumber
import Structures.Lines
import Structures.Words
import Constants.StringWorks


data Question = Question {
    modifier :: QModifier,
    fields :: [QField]
} deriving (Eq, Show)


data Tour = Tour {
    quests :: [Question],
    comment :: Maybe Comment
    } deriving (Eq, Show)


newtype Comment = Comment { unComment :: String } deriving (Eq, Show)


instance ShowNatural Comment where
    showNatural = unComment


instance HasToken Question where
    tokenOf = const "?"

instance HasToken Comment where
    tokenOf = const "#"

data QFieldType = QText | QAnswer | QEquiv | QNotEquiv | QComment | QSource | QAuthor deriving (Eq, Enum, Ord)

instance Show QFieldType where
    show QText = "?"
    show QEquiv = "="
    show QAuthor = "@"
    show QSource = "^"
    show QAnswer = "!"
    show QComment = "/"
    show QNotEquiv = "!="

allQFTs :: [QFieldType]
allQFTs = [toEnum 0 ..]

instance ShowNatural QFieldType where
    showNatural = tokenToString . show 

instance HasToken QFieldType where
    tokenOf = show


data QField = QField QFieldType [Line] deriving (Eq, Show)
instance ShowNatural QField where
    showNatural (QField t _) = showNatural t
instance Ord QField where
    (QField t _) <= (QField s _) = fromEnum t <= fromEnum s




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


testQuestion :: Question
testQuestion = Question (Hard 1) [QField QText [Line [RegWord "question"]]]