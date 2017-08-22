module QNumber where

import           Inline
import           Text.Parsec
import           Text.Parsec.String


type QModifier = Either String Integer
type QModifierM = Maybe QModifier

{-|
    Basic test to output question number
    n number to put if there are now identifiers
    Right newInt - the questions now count from this number
    Left tempStr - temporary question number
-}
printQN :: Integer -> QModifierM -> String
printQN n Nothing               = show n
printQN _ (Just (Right newInt)) = show newInt
printQN _ (Just (Left tempStr)) = tempStr

someQNS :: [QModifierM]
someQNS = [Nothing, Nothing, Just $ Right 12, Just $ Left "q?"]

nextNumber :: Integer -> QModifierM -> Integer
nextNumber n Nothing               = n + 1
nextNumber _ (Just (Right newInt)) = newInt
nextNumber n (Just (Left _))       = n + 1

scaa' :: [Integer] -> [QModifierM] -> [Integer]
scaa' ns []         = ns
scaa' [] qs         = scaa' [0] qs
scaa' (n:ns) (q:qs) = scaa' (nextNumber n q : n : ns) qs

scaa :: [Integer]
scaa = reverse $ scaa' [1] someQNS

qSoftReset :: Parser QModifier
qSoftReset = do
  () <$ char '№'
  c <- noneOf "№"
  s <- many1 $ noneOf "\n"
  () <$ char '\n'
  return $ Left (c : s)

qHardReset :: Parser QModifier
qHardReset = do
  () <$ string "№№"
  s <- decimal
  () <$ char '\n'
  return $ Right s


questModifier :: Parser QModifierM
questModifier = optionMaybe $ try qHardReset <|> try qSoftReset


{--!
-- Show question number modifier
--}
showQ :: QModifierM -> String
showQ Nothing               = ""
showQ (Just (Right newInt)) = "(Номер вопроса)" ++ show newInt ++ "\n"
showQ (Just (Left tempStr)) = "(Номер вопроса)" ++ tempStr ++ "\n"
