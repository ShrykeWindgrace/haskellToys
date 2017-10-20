module Parsers.QN (qSoftReset, qHardReset, questModifier)


where


import           Constants.StringWorks (parsingToken)
import           Parsers.InlineSpace   (skipSpaces)
import           Parsers.Lines         (pLineInner)
import           Parsers.Primitives    (decimal)
import           Parsers.Tech          (Parser)
import           Structures.Lines      (Line (..), ListLines (..))
import           Structures.QNumber    (QModifier (..), QModifierM)
import           Text.Megaparsec       (optional, try, (<|>))
import           Text.Megaparsec.Char  (string)


qHardReset :: Parser QModifier
qHardReset = Hard <$> (string (parsingToken Hard{}) >> skipSpaces >> decimal)


qSoftReset :: Parser QModifier
qSoftReset = Soft . showHumanReadable <$> (string (parsingToken Soft{}) >> skipSpaces >> pLineInner) where
    showHumanReadable :: Line -> String
    showHumanReadable (Line list) = unwords (show <$> list)
    showHumanReadable (ListLinesStr (ListLines list)) = list >>= showHumanReadable -- todo: test

questModifier :: Parser QModifierM
questModifier = optional $ try qHardReset <|> try qSoftReset
