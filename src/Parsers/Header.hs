module Parsers.Header (parseEditor, headerParsers) where


import           Constants.StringWorks (parsingToken)
import           Parsers.InlineSpace   (skipSpaces)
import           Parsers.Lines         (pLineInner)
import           Structures.Header     (HeaderItem (..), HeaderItemType (..),
                                        allHeaderItemTypes)
import           Text.Megaparsec       (dbg)
import           Text.Megaparsec.Char  (string)
import           Parsers.Tech          (Parser)


parseHeaderGen :: HeaderItemType -> Parser HeaderItem
parseHeaderGen t = HeaderItem t <$>
    (string (parsingToken t) >> skipSpaces >> pLineInner)


headerParsers :: [Parser HeaderItem]
headerParsers = map (dbg "header") (parseHeaderGen <$> allHeaderItemTypes)


parseEditor :: Parser HeaderItem
parseEditor = head headerParsers
