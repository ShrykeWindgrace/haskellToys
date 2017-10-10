module Parsers.Lines (pLine, pLines, pLineExternal, parseQFall) where

import           Constants.StringWorks  (parsingToken, tokenList)
import           Parsers.Inline
import           Parsers.InlineSpace    (skipSpaces)
import           Structures.Lines
import           Structures.Quest
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)


pLine :: Parser Line
pLine = Line <$> (oneWord `sepEndBy1` skipSpaces) <* (eof <|> () <$ eol)


pLineExternal :: Parser Line
pLineExternal = try (lookAhead $ satisfy (`notElem` tokenList)) >> pLine


pLines :: Parser ListLines
pLines = ListLines <$> some (char '-' >> skipSpaces >> pLine)


parseQF :: QFieldType -> Parser QField
parseQF qft = QField qft <$> do
    _ <- string (parsingToken qft)
    skipSpaces
    l <- pLine
    ls <- many pLineExternal
    return (l:ls)

parseQFall :: Parser QField
parseQFall = choice (parseQF <$> QNotEquiv : allQFTs) -- hack to be fixed
