module Parsers.Header (parseEditor, headerParsers) where

import           Constants.StringWorks  (parsingToken)
import           Parsers.InlineSpace    (skipSpaces)
import           Parsers.Lines          (pLineInner)
import           Structures.Header      (HeaderItem (..), HeaderItemType (..),
                                         allHeaderItemTypes)
import           Text.Megaparsec        (choice, eof, eol, string, dbg)
import           Data.Text              (pack)
import           Text.Megaparsec.String (Parser)

parseHeaderGen :: HeaderItemType -> Parser HeaderItem
parseHeaderGen t = HeaderItem t . pack . show <$>
    (string (parsingToken t) >> skipSpaces >> pLineInner <* choice [eof, () <$ eol])


headerParsers :: [Parser HeaderItem]
headerParsers = map (dbg "header") (parseHeaderGen <$> allHeaderItemTypes)


parseEditor :: Parser HeaderItem
parseEditor = head headerParsers
