{-# LANGUAGE OverloadedStrings #-}
module Structures.QNumber ( QModifier(..), QModifierM, isSoft)


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

    parsingToken Soft{} = "№" -- the corresponding token in *.4s
    parsingToken Hard{} = "№№" -- the corresponding token in *.4s

    cssClass = const "qmodifier"--the corresponding css class


type QModifierM = Maybe QModifier

isSoft :: QModifier -> Bool
isSoft (Soft _) = True
isSoft _        = False
