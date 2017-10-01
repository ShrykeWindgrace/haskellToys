module Parsers.Header (parseEditor) where

import           Constants.StringWorks  (parsingToken)
import           Parsers.InlineSpace    (skipSpaces)
import           Parsers.Lines          (pLine)
import           Structures.Header      (HeaderItem (..), HeaderItemType (..),
                                         allHeaderItemTypes)
import           Text.Megaparsec        (choice, eof, eol, string)
-- import           Text.Parsec.Perm
import           Data.Text              (pack)
import           Text.Megaparsec.String (Parser)

parseHeaderGen :: HeaderItemType -> Parser HeaderItem
parseHeaderGen t = HeaderItem t . pack . show <$>
    (string (parsingToken t) >> skipSpaces >> pLine <* choice [eof, () <$ eol])


headerParsers :: [Parser HeaderItem]
headerParsers = parseHeaderGen <$> allHeaderItemTypes


parseEditor :: Parser HeaderItem
parseEditor = head headerParsers
