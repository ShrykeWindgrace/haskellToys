module Parsers.InlineSpace
  ( spaces', blankLine
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec
spaces' :: Parser String
spaces' = many (oneOf " \t")


blankLine :: Parser ()
blankLine = () <$ (spaces' >> eol <?> "\"\\n\" or \"\\r\\n\"")
