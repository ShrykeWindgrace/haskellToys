module Parsers.QN where

import           Parsers.Lines
import           Parsers.InlineSpace
import           Parsers.Primitives (decimal)
import           Parsers.Tech
import           Structures.QNumber
import           Text.Parsec
import           Text.Parsec.String


qHardReset :: Parser QModifier
qHardReset = Hard <$> (string "№№" >> lexeme decimal)

qSoftReset :: Parser QModifier
qSoftReset = Soft . show <$> (char '№' >> skipSpaces >> pLine) -- todo: use human-readable version of Show


questModifier :: Parser QModifierM
questModifier = optionMaybe $ try qHardReset <|> try qSoftReset
