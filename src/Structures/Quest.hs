{-# LANGUAGE OverloadedStrings #-}
module Structures.Quest where

import           Constants.StringWorks (Element4s, cssClass, parsingToken,
                                        showNatural, tokenToString)
import           Data.Maybe            (fromJust, isJust, isNothing)
import           Data.Text             (append, pack)
import           Structures.Header     (HeaderItem)
import           Structures.Lines      (Line (..))
import           Structures.QNumber    (QModifier (..), QModifierM, isSoft)
import           Structures.Words (OneWord(..), ILink(..))


data Question = Question {
    modifier :: QModifierM,
    fields   :: [QField]
} deriving (Eq, Show)


data Tour = Tour {
    quests    :: [Question],
    comment   :: Maybe Comment,
    tModifier :: QModifierM
    } deriving (Eq, Show)


data Tournament = Tournament  {
    header     :: [HeaderItem],
    commentTNT :: Maybe Comment,
    tours      :: [Tour]
    } deriving (Eq, Show)



newtype Comment = Comment { unComment :: Line } deriving (Eq, Show)


instance Element4s Comment where
    showNatural = const "" -- the token shown in rendering
    parsingToken = const "#" -- the corresponding token in *.4s
    cssClass = const "comment"  --the corresponding css class



instance Element4s Question where
    showNatural = const "" -- the token shown in rendering
    parsingToken = const (parsingToken QText) -- the corresponding token in *.4s
    cssClass = const "question"  --the corresponding css class


instance Element4s Tour where
    showNatural = const "Тур" -- the token shown in rendering
    parsingToken = const "===" -- the corresponding token in *.4s
    cssClass = const "tour"  --the corresponding css class

instance Element4s Tournament where
    showNatural = const "" -- the token shown in rendering
    parsingToken = const "" -- the corresponding token in *.4s
    cssClass = const "tournament"  --the corresponding css class



data QFieldType = QText | QAnswer | QEquiv | QNotEquiv | QComment | QSource | QAuthor deriving (Eq, Enum, Ord, Show, Bounded)



allQFTs :: [QFieldType]
allQFTs = [toEnum 0 ..]


instance Element4s QFieldType where
    showNatural = tokenToString . parsingToken -- the token shown in rendering
    -- the corresponding token in *.4s
    parsingToken QText     = "?"
    parsingToken QEquiv    = "="
    parsingToken QAuthor   = "@"
    parsingToken QSource   = "^"
    parsingToken QAnswer   = "!"
    parsingToken QComment  = "/"
    parsingToken QNotEquiv = "!="

    cssClass = pack . show  --the corresponding css class


data QField = QField QFieldType [Line] deriving (Eq, Show)


instance Element4s QField where
    showNatural (QField t _) = showNatural t-- the token shown in rendering
    parsingToken (QField t _) = parsingToken t -- the corresponding token in *.4s
    cssClass (QField t _) = cssClass  t `append` "Content"--the corresponding css class

instance Ord QField where
    (QField t _) <= (QField s _) = fromEnum t <= fromEnum s




instance Read QFieldType where
    readsPrec _ str = [(charToFT str, "")] where
        charToFT :: String -> QFieldType
        charToFT "?"  = QText -- "Вопрос"
        charToFT "="  = QEquiv -- "Зачёт"
        charToFT "@"  = QAuthor -- "Автор(ы)"
        charToFT "^"  = QSource -- "Источник(и)"
        charToFT "!"  = QAnswer -- "Ответ"
        charToFT "/"  = QComment -- "Комментарии"
        charToFT "!=" = QNotEquiv -- "незачёт"
        charToFT _    = error "illegal question field type identifier"


testQuestion :: Question
testQuestion = Question (Just $ Hard 1) [
    QField QText [Line [RegWord "question", ILinkStr $ ILink "local.jpg" (Just 600) Nothing]],
    QField QAnswer [Line [StressedWord "ур" 'а' ""]]
    ]


enumerateQuestions :: Integer -> [Question] -> [Question]
enumerateQuestions _ [] = []
enumerateQuestions n (q:qs)
    | isNothing (modifier q) = q {modifier = Just $ Hard n} : enumerateQuestions (n+1) qs
    | isSoft (fromJust (modifier q)) = q : enumerateQuestions (n+1) qs
    | otherwise = let (Hard newN) = fromJust (modifier q) in q : enumerateQuestions (newN+1) qs


enumerateTours :: Tournament -> Tournament
enumerateTours a@(Tournament _ _ _tours) =
  a {tours = newTours} where
    newTours =  zipWith update [1..] _tours where
      update n _tour' = if isJust (tModifier _tour) then _tour else _tour {tModifier = Just (Hard n)} where
        _tour = _tour' {quests = enumerateQuestions 1 (quests _tour')}
