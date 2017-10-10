module Parsers.InlineSpace
  (
   -- spaces',
    -- blankLine,
    blankLines,
    -- spaceChar,
    skipSpaces,
    -- nonSpaceChar
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec

-- spaceChar' :: Parser Char
-- spaceChar' = oneOf " \t"


skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ')


blankLine :: Parser ()
blankLine = () <$ (skipSpaces >> eol) <|> eof


blankLines :: Parser ()
blankLines = eof <|> () <$ some blankLine
