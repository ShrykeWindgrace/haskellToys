module Parsers.Lines (pLine, pLines) where

import           Parsers.Inline
import           Parsers.InlineSpace    (skipSpaces)
import           Structures.Lines
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)

pLine :: Parser Line
pLine = Line <$> oneWord `sepEndBy1` skipSpaces


pLines :: Parser ListLines
pLines = ListLines <$> (char '-' >> pLine) `sepEndBy1` (skipSpaces >> char '\n')
 --  separated and optionally ended by
