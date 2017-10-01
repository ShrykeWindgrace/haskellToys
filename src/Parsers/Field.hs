module Parsers.Field where

import           Structures.Quest       (QFieldType)
import           Text.Megaparsec        (string, try)
import           Text.Megaparsec.String (Parser)


fieldType :: String -> Parser QFieldType
fieldType str = try (string str) >> return (read str)
