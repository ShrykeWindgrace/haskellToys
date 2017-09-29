module Parsers.Field where

import           Structures.Quest   (QFieldType)
import           Text.Parsec        (string, try)
import           Text.Parsec.String (Parser)


fieldType :: String -> Parser QFieldType
fieldType str = try (string str) >> return (read str)
