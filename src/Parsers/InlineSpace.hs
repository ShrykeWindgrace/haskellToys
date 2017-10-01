module Parsers.InlineSpace
  (
   -- spaces',
    -- blankLine,
    -- blankLines,
    -- spaceChar,
    skipSpaces,
    -- nonSpaceChar
  ) where

-- import           Parsers.Types
import           Text.Megaparsec
-- import           Text.Megaparsec.Char
import           Text.Megaparsec.String

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec

-- spaceChar' :: Parser Char
-- spaceChar' = oneOf " \t"


skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ')


-- blankLine :: Parser Char
-- blankLine = try $ skipSpaces >> eol


-- blankLines :: Parser String
-- blankLines = some blankLine
