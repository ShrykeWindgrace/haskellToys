module Parsers.InlineSpace
  ( spaces', blankLine
  ) where

import           Text.Parsec        (endOfLine, many, oneOf, (<?>))
import           Text.Parsec.String (Parser)

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec
spaces' :: Parser String
spaces' = many (oneOf " \t")


blankLine :: Parser ()
blankLine = () <$ (spaces' >> endOfLine <?> "\"\\n\" or \"\\r\\n\"")
