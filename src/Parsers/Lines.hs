module Parsers.Lines (pLineInner, pLines, pLineExternal, parseQFall) where

import           Constants.StringWorks  (parsingToken, tokenList)
import           Parsers.Inline
import           Parsers.InlineSpace    (skipSpaces)
import           Structures.Lines
import           Structures.Quest
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)


pLineInner :: Parser Line
pLineInner = Line <$> (oneWord `sepEndBy1` skipSpaces) <* (eof <|> () <$ eol) -- consumes line endings


-- line with token, i.e. it belongs to a previously defined field
pLineExternal :: Parser Line
pLineExternal = (ListLinesStr <$> pLines) <|> (try (lookAhead $ satisfy (`notElem` tokenList)) >> pLineInner)


pLines :: Parser ListLines
pLines = ListLines <$> some (char '-' >> skipSpaces >> pLineInner)




parseQF :: QFieldType -> Parser QField
parseQF qft = QField qft <$> do
    _ <- string (parsingToken qft)
    skipSpaces
    _ <- optional newline
    some pLineExternal

parseQFall :: Parser QField
parseQFall = choice (parseQF <$> QNotEquiv : allQFTs) -- hack to be fixed
