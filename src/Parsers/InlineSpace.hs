module Parsers.InlineSpace
  (
   -- spaces',
    blankLine,
    blankLines,
    -- spaceChar,
    wordEnd,
    skipSpaces,
    skipSpaces1,
    -- nonSpaceChar
  ) where

import           Text.Megaparsec (eof, (<|>), skipMany, some, choice, try, lookAhead)
import           Text.Megaparsec.Char (char, eol, spaceChar)
import           Parsers.Tech (Parser)

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec

-- spaceChar' :: Parser Char
-- spaceChar' = oneOf " \t"


skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ')

skipSpaces1 :: Parser ()
skipSpaces1 = (char ' ' <|> char '\n') >> skipMany (char ' ')

blankLine :: Parser ()
blankLine = () <$ (skipSpaces >> eol) <|> eof


blankLines :: Parser ()
blankLines = eof <|> () <$ some blankLine


{-
    Parser for "this is the end of the current word, but we do not consume that end"
-}
wordEnd :: Parser ()
wordEnd = try $ lookAhead $ choice [eof, () <$ spaceChar]


