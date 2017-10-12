module Parsers.QN (qSoftReset, qHardReset, questModifier)


where


import           Constants.StringWorks  (parsingToken)
import           Parsers.InlineSpace    (skipSpaces)
import           Parsers.Lines          (pLineInner)
import           Parsers.Primitives     (decimal)
import           Structures.Lines
import           Structures.QNumber     (QModifier (..), QModifierM)
import           Text.Megaparsec        (optional, string, try, (<|>))
import           Text.Megaparsec.String (Parser)


qHardReset :: Parser QModifier
qHardReset = Hard <$> (string (parsingToken Hard{}) >> skipSpaces >> decimal)


qSoftReset :: Parser QModifier
qSoftReset = Soft . showHumanReadable <$> (string (parsingToken Soft{}) >> skipSpaces >> pLineInner) where
    showHumanReadable :: Line -> String
    showHumanReadable (Line list) = unwords (show <$> list)
    showHumanReadable _ = error "lists are not allowed in question numbers"

questModifier :: Parser QModifierM
questModifier = optional $ try qHardReset <|> try qSoftReset
