module Parsers.QN where

import           Parsers.Inline     (decimal, rawLine)
import           Parsers.Tech
import           Structures.QNumber
import           Text.Parsec
import           Text.Parsec.String



qSoftReset :: Parser QModifier
qSoftReset = do
  () <$ char '№'
  c <- noneOf "№"
  s <- many1 $ noneOf "\n"
  () <$ char '\n'
  return $ Soft (c : s)

qHardReset :: Parser QModifier
qHardReset = do
  () <$ string "№№"
  s <- decimal
  () <$ char '\n'
  return $ Hard s


qHardReset' :: Parser QModifier
qHardReset' = Hard <$> between (string "№№") (lexeme endOfLine) decimal

qSoftReset' :: Parser QModifier
qSoftReset' = Soft <$> between (char '№') (lexeme endOfLine) rawLine -- TODO use usualLine


questModifier :: Parser QModifierM
questModifier = optionMaybe $ try qHardReset <|> try qSoftReset



