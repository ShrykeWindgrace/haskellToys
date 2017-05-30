module Qs (parseQ, combiQ, blankLine) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)


qText :: Parser String
qText = do
  void $ char '?'
  spaces 
  s <- regText
  return $ "Q:" ++ s

parseQ :: Parser String
parseQ = qText

regText :: Parser String
regText = many1 anyChar -- >>= return "RT"
   

combiQ :: Parser String
combiQ = parseQ <|> blankLine <|> regText <|> newline1

blankLine :: Parser String
blankLine = do
  spaces
  eof
  return "blank"

newline1 :: Parser String
newline1 = eof >> return "blank-ish" 
