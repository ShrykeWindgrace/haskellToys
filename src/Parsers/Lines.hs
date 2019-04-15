module Parsers.Lines (pLineInner, pLines, pLineExternal, parseQFall) where

import           Constants.StringWorks (parsingToken, tokenList)
import           Parsers.Inline (oneWord)
import           Parsers.InlineSpace   (skipSpaces, skipSpaces1)
import           Parsers.Tech          (Parser)
import           Structures.Lines (Line(..), ListLines(..))
import           Structures.Quest (QField(..), QFieldType(..), allQFTs)
import           Text.Megaparsec       (choice, eof, lookAhead, optional,
                                        sepEndBy1, some, try, (<|>), satisfy)
import           Text.Megaparsec.Char  (char, eol, newline, string)


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
    skipSpaces1
    _ <- optional newline
    some pLineExternal

parseQFall :: Parser QField
parseQFall = choice (parseQF <$> QNotEquiv : allQFTs) -- hack to be fixed
