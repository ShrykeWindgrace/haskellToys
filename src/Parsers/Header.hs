module Parsers.Header (parse, parseEditor) where

import           Parsers.InlineSpace
import           Render.StringWorks  (edLine)
import           Structures.Header
import           Text.Parsec
import           Text.Parsec.String

parseEditor :: Parser Editor
parseEditor = Editor <$> (string edLine >> skipSpaces >> manyTill anyChar newline)
