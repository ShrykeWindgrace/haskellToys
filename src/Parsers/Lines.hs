module Parsers.Lines (pLine, pLines) where

import           Parsers.Inline
import           Parsers.InlineSpace    (skipSpaces)
import           Structures.Lines
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)

pLine :: Parser Line
pLine = Line <$> (oneWord `sepEndBy1` skipSpaces) <* (eof <|> () <$ eol)


pLines :: Parser ListLines
pLines = ListLines <$> some (char '-' >> skipSpaces >> pLine)
