module QNumber where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
-- import Data.List


type QModifier = Maybe (Either String Int)


{-|
    Basic test to output question number
    n number to put if there are now identifiers
    Right newInt - the questions now count from this number
    Left tempStr - temporary question number
-}
printQN :: Int -> QModifier -> String 
printQN n Nothing = show n
printQN _ (Just (Right newInt)) = show newInt
printQN _ (Just (Left tempStr)) = tempStr

someQNS :: [QModifier]
someQNS = [Nothing, Nothing, Just $ Right 12, Just $ Left "q?"]

nextNumber :: Int -> QModifier -> Int
nextNumber n Nothing = succ n
nextNumber _ (Just (Right newInt)) = newInt
nextNumber n (Just (Left _)) = succ n


scaa' :: [Int] -> [QModifier] -> [Int]
scaa' ns [] = ns 
scaa' [] qs = scaa' [0] qs 
scaa' (n:ns) (q:qs) = scaa' (nextNumber n q : n: ns)  qs

scaa :: [Int]
scaa = reverse $ scaa' [1] someQNS


qSoftReset :: Parser (Either String Int)
qSoftReset = do
    void $ char '№'
    c <- noneOf "№"
    s <- many1 $ noneOf "\n"
    void $ char '\n'
    return $ Left (c:s)


qHardReset :: Parser (Either String Int)
qHardReset = do
    void $ string "№№"
    void $ many $ char ' '
    s <- many1 digit
    void $ char '\n'  
    return $ Right $ read s


questModifier :: Parser QModifier
questModifier = optionMaybe $ try qHardReset <|> try qSoftReset


showQ :: QModifier -> String
showQ Nothing = ""
showQ (Just (Right newInt)) = show newInt ++ "\n"
showQ (Just (Left tempStr)) = tempStr ++ "\n"