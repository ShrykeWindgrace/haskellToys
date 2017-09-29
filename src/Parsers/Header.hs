module Parsers.Header (parseEditor) where

import           Constants.StringWorks (parsingToken)
import           Parsers.InlineSpace   (skipSpaces)
import           Parsers.Lines         (pLine)
import           Structures.Header     (HeaderItem (..), HeaderItemType (..),
                                        allHeaderItemTypes)
import           Text.Parsec           (choice, endOfLine, eof, string)
-- import           Text.Parsec.Perm
import           Text.Parsec.String    (Parser)


parseHeaderGen :: HeaderItemType -> Parser HeaderItem
parseHeaderGen t = HeaderItem t . show <$>
    (string (parsingToken t) >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine])


headerParsers :: [Parser HeaderItem]
headerParsers = parseHeaderGen <$> allHeaderItemTypes


parseEditor :: Parser HeaderItem
parseEditor = head headerParsers
