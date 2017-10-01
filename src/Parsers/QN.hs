module Parsers.QN (qSoftReset, qHardReset, questModifier)


where


import           Constants.StringWorks (parsingToken)
import           Parsers.InlineSpace   (skipSpaces)
import           Parsers.Lines         (pLine)
import           Parsers.Primitives    (decimal)
import           Structures.QNumber    (QModifier (..), QModifierM)
import           Text.Parsec           (optionMaybe, string, try, (<|>))
import           Text.Parsec.String    (Parser)


qHardReset :: Parser QModifier
qHardReset = Hard <$> (string (parsingToken $ Hard 0) >> skipSpaces >> decimal)


qSoftReset :: Parser QModifier
qSoftReset = Soft . show <$> (string (parsingToken $ Soft "") >> skipSpaces >> pLine) -- todo: use human-readable version of Show


questModifier :: Parser QModifierM
questModifier = optionMaybe $ try qHardReset <|> try qSoftReset
