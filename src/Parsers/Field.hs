module Parsers.Field where

import Text.Parsec
import Text.Parsec.String
import Structures.Quest


fieldType :: String -> Parser QFieldType
fieldType str = try (string str) >> return (charToFT str)
