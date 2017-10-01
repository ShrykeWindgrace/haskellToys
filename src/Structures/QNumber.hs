{-# LANGUAGE OverloadedStrings #-}
module Structures.QNumber ( QModifier(..), QModifierM)


where


import           Constants.StringWorks (Element4s, cssClass, parsingToken,
                                        showNatural)


data QModifier = Soft String | Hard Integer
    deriving (Eq)


instance Show QModifier where
    show (Soft line) = line
    show (Hard int)  = show int


instance Element4s QModifier where
    showNatural = const "Вопрос"-- the token shown in rendering

    parsingToken (Soft _) = "№" -- the corresponding token in *.4s
    parsingToken (Hard _) = "№№" -- the corresponding token in *.4s

    cssClass = const "qmodifier"--the corresponding css class


type QModifierM = Maybe QModifier


-- showQ :: QModifierM -> String
-- showQ Nothing   = ""
-- showQ (Just qm) = "(Номер вопроса)" ++ show qm ++ "\n"


-- nextNumber :: Integer -> QModifierM -> Integer
-- nextNumber _ (Just (Hard newInt)) = newInt
-- nextNumber n _                    = n + 1
