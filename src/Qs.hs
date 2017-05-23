module Qs (parseQ, combiQ, blankLine) where

import Text.Parsec
import Text.Parsec.String

qText :: Parser String
qText = do
  char '?'
  spaces 
  s <- regText
  return $ "Q:" ++ s

parseQ = qText

regText :: Parser String
regText = many1 anyChar -- >>= return "RT"
   

combiQ :: Parser String
combiQ = parseQ <|> blankLine <|> regText <|> newline1

blankLine :: Parser String
blankLine = do
  char ' '
  {-endOfLine  -}
  return "blank"

newline1 = (newline >> return "blanche") <?> "wtf"
