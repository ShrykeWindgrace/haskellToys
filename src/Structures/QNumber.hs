module Structures.QNumber where


data QModifier = Soft String | Hard Integer
  deriving (Eq)

instance Show QModifier where
  show (Soft line) = line
  show (Hard int)  = show int


type QModifierM = Maybe QModifier


showQ :: QModifierM -> String
showQ Nothing   = ""
showQ (Just qm) = "(Номер вопроса)" ++ show qm ++ "\n"



nextNumber :: Integer -> QModifierM -> Integer
nextNumber _ (Just (Hard newInt)) = newInt
nextNumber n _                    = n + 1
