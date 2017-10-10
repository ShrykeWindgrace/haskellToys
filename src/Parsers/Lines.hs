module Parsers.Lines (pLine, pLines, pLineExternal) where

import           Constants.StringWorks  (tokenList)
import           Parsers.Inline
import           Parsers.InlineSpace    (skipSpaces)
import           Structures.Lines
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)


pLine :: Parser Line
pLine = Line <$> (oneWord `sepEndBy1` skipSpaces) <* (eof <|> () <$ eol)


pLineExternal :: Parser Line
pLineExternal = try (lookAhead $ satisfy (`notElem` tokenList)) >> pLine



pLines :: Parser ListLines
pLines = ListLines <$> some (char '-' >> skipSpaces >> pLine)
