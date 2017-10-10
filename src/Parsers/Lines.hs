module Parsers.Lines (pLine, pLines, pLineExternal, parseQF) where

import           Constants.StringWorks  (tokenList, parsingToken)
import           Parsers.Inline
import           Parsers.InlineSpace    (skipSpaces)
import           Structures.Lines
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)
import Structures.Quest


pLine :: Parser Line
pLine = Line <$> (oneWord `sepEndBy1` skipSpaces) <* (eof <|> () <$ eol)


pLineExternal :: Parser Line
pLineExternal = try (lookAhead $ satisfy (`notElem` tokenList)) >> pLine


pLines :: Parser ListLines
pLines = ListLines <$> some (char '-' >> skipSpaces >> pLine)


-- toks = string <$> parsingToken <$> allQFTs


parseQF :: QFieldType -> Parser QField
parseQF qft = QField qft <$> do
    _ <- string (parsingToken qft)
    skipSpaces
    l <- pLine
    ls <- many pLineExternal
    return (l:ls)
