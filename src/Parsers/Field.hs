module Parsers.Field (fieldType) where

import           Parsers.Tech         (Parser)
import           Structures.Quest     (QFieldType)
import           Text.Megaparsec      (try, (<|>))
import           Text.Megaparsec.Char (string, char)
import           Constants.StringWorks (parsingToken)



fieldType :: QFieldType -> Parser QFieldType
fieldType qft = try (string str) >> (char ' ' <|> char '\n') >> return qft where
    str = parsingToken qft
