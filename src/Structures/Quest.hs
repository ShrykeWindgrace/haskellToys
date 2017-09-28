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


instance Element4s Comment where
    showNatural = unComment -- the token shown in rendering
    parsingToken = const "#" -- the corresponding token in *.4s
    cssClass = const "comment"  --the corresponding css class
    


instance Element4s Question where
    showNatural = const "" -- the token shown in rendering
    parsingToken = const (parsingToken QText) -- the corresponding token in *.4s
    cssClass = const "question"  --the corresponding css class


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


instance Element4s QFieldType where
    showNatural = tokenToString . show -- the token shown in rendering
    parsingToken = show -- the corresponding token in *.4s
    cssClass = undefined  --the corresponding css class


data QField = QField QFieldType [Line] deriving (Eq, Show)


instance Element4s QField where
    showNatural (QField t _) = showNatural t-- the token shown in rendering
    parsingToken (QField t _) = parsingToken t -- the corresponding token in *.4s
    cssClass (QField t _) = cssClass  t ++ "Content"--the corresponding css class

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