module Parsers.InlineSpace
  ( spaces',
    blankLine,
    blankLines,
    spaceChar,
    skipSpaces
  ) where

import           Text.Parsec
import           Text.Parsec.String (Parser)

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec
spaces' :: Parser String
spaces' = many spaceChar

spaceChar :: Parser Char
spaceChar = oneOf " \t"

skipSpaces :: Parser ()
skipSpaces = skipMany spaceChar

blankLine :: Parser Char
blankLine = try $ skipSpaces >> newline

blankLines :: Parser [Char]
blankLines = many1 blankLine
