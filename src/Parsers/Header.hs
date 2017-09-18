module Parsers.Header (parse, parseEditor) where

import           Parsers.InlineSpace
import           Render.StringWorks  (edLine)
import           Structures.Header
import           Text.Parsec
import           Text.Parsec.String

parseEditor :: Parser Editor
parseEditor = Editor <$> do
    string edLine
    skipSpaces
    c <- nonSpaceChar
    cs <- manyTill anyChar newline
    return (c:cs)
