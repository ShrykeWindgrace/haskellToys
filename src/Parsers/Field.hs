module Parsers.Field where

import           Structures.Quest
import           Text.Parsec
import           Text.Parsec.String


fieldType :: String -> Parser QFieldType
fieldType str = try (string str) >> return (read str)
