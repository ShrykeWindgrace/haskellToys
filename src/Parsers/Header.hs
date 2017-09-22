module Parsers.Header (parseEditor) where

import           Parsers.InlineSpace
import           Parsers.Lines
import           Render.StringWorks
import           Structures.Header
import           Text.Parsec
-- import           Text.Parsec.Perm
import           Text.Parsec.String


parseHeaderGen :: HeaderItemType -> Parser HeaderItem
parseHeaderGen t = HeaderItem t . showNatural <$> 
    (string (tokenOf t) >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine])


headerItems :: [HeaderItemType]
headerItems = [Editor, Title, TDate]


headerParsers :: [Parser HeaderItem]
headerParsers = parseHeaderGen <$> headerItems


parseEditor :: Parser HeaderItem
parseEditor = head headerParsers
