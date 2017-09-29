module Parsers.QN where

import           Constants.StringWorks
import           Parsers.InlineSpace
import           Parsers.Lines
import           Parsers.Primitives    (decimal)
import           Parsers.Tech
import           Structures.QNumber
import           Text.Parsec
import           Text.Parsec.String

qHardReset :: Parser QModifier
qHardReset = Hard <$> (string (parsingToken $ Hard 0) >> lexeme decimal)

qSoftReset :: Parser QModifier
qSoftReset = Soft . show <$> (string (parsingToken $ Soft "") >> skipSpaces >> pLine) -- todo: use human-readable version of Show


questModifier :: Parser QModifierM
questModifier = optionMaybe $ try qHardReset <|> try qSoftReset
