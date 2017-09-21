module Parsers.Header (parseEditor) where

import           Parsers.InlineSpace
import           Parsers.Lines
import           Render.StringWorks  
import           Structures.Header
import           Text.Parsec
import           Text.Parsec.String

parseEditor :: Parser Editor
parseEditor = Editor . showNatural <$> do
    string edLine >> skipSpaces >> pLine <* choice [eof, () <$ endOfLine]
