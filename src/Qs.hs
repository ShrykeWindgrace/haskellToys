module Qs (parseQ) where

import Text.Parsec
import Text.Parsec.String

qText :: Parser String
qText = do
  char '?'
  spaces 
  s <- many anyChar--alphaNum
  return s

parseQ = qText
