module Parsers.Field (fieldType) where

import           Parsers.Tech         (Parser)
import           Structures.Quest     (QFieldType)
import           Text.Megaparsec      (try, (<|>))
import           Text.Megaparsec.Char (string, char)


fieldType :: String -> Parser QFieldType
fieldType str = try (string str) >> (char ' ' <|> char '\n') >> return (read str)
