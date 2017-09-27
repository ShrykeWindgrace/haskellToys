module Parsers.Header (parseEditor) where

import           Parsers.InlineSpace
import           Parsers.Lines
import           Constants.StringWorks
import           Structures.Header
import           Text.Parsec
-- import           Text.Parsec.Perm
import           Text.Parsec.String


parseHeaderGen :: HeaderItemType -> Parser HeaderItem
parseHeaderGen t = HeaderItem t . showNatural <$> 
    (string (tokenOf t) >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine])


headerParsers :: [Parser HeaderItem]
headerParsers = parseHeaderGen <$> allHeaderItemTypes


parseEditor :: Parser HeaderItem
parseEditor = parseHeaderGen Editor
