module Parsers.Field where

import           Structures.Quest
import           Text.Megaparsec        
import           Text.Megaparsec.String


fieldType :: String -> Parser QFieldType
fieldType str = string str >> return (charToFT str)
