module Parsers.Header (parseEditor) where

import           Parsers.InlineSpace
import           Parsers.Lines
import           Render.StringWorks
import           Structures.Header
import           Text.Parsec
import           Text.Parsec.Perm
import           Text.Parsec.String

parseEditor :: Parser HeaderItem
parseEditor = Editor . showNatural <$> do
    string (tokenOf (Editor undefined)) >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine]


-- parseDate :: Parser TDate
-- parseDate = TDate . showNatural <$> do
--     string dateLine >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine]


-- parseTitle :: Parser Title
-- parseTitle = Title . showNatural <$> do
--     string titleLine >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine]

-- TODO these three parsers are one and the same up to their token; should find a way to refactor them
--

