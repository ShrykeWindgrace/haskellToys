module Parsers.QN where

import           Parsers.Inline     (decimal, rawLine)
import           Parsers.Tech
import           Structures.QNumber
import           Text.Megaparsec        
import           Text.Megaparsec.String



qSoftReset :: Parser QModifier
qSoftReset = do
  () <$ char '№'
  c <- noneOf "№"
  s <- some $ noneOf "\n"
  () <$ char '\n'
  return $ Soft (c : s)

qHardReset :: Parser QModifier
qHardReset = do
  () <$ string "№№"
  s <- decimal
  () <$ char '\n'
  return $ Hard s


qHardReset' :: Parser QModifier
qHardReset' = Hard <$> (string "№№" >> lexeme decimal)

qSoftReset' :: Parser QModifier
qSoftReset' = Soft <$> between (char '№') (lexeme eol) rawLine -- TODO use usualLine


questModifier :: Parser QModifierM
questModifier = optional $ qHardReset <|> try qSoftReset
