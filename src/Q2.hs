module Q2  (someQQ2) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

tokenList :: String
tokenList = "?-#\n"

inlineText :: Parser String
inlineText = do
  c <- noneOf tokenList
  s <- many1 (noneOf "\n") 
  void (char '\n') <|> eof 
  return (c:s)
  <?> "inlineText"


tokenLines :: Char -> Parser String
tokenLines c = do
  char c
  ss <- many1 $ try inlineText
  return $ '(':c:')': concat ss
  <?> ("line with token " ++ [c])



answerText :: Parser String
answerText = tokenLines '-'

questText :: Parser String
questText = tokenLines '?'


fullQuestText :: Parser String
fullQuestText = do
  q <- questText
  a <- answerText
  void endOfLine <|> eof
  return $ q ++ a
  <?> "fullQuestText"


questGrammar :: Parser String
questGrammar = do
  s <- many1 fullQuestText
  return $ concat s 



someQQ2 :: IO()
someQQ2 = case parse questGrammar "" "? q1\n- a1\n\n? q2 \n- a2" of 
      Right answ -> print $ "Ok: " ++   answ
      Left err -> print err
