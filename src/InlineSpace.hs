module InlineSpace
  ( spaces'
  ) where

import Text.Parsec (many, oneOf)
import Text.Parsec.String (Parser)

-- If we ever want to deal with other whitespace characters, we should implement this parser in the same spirit as "isSpace"
-- method in Parsec
spaces' :: Parser ()
spaces' = () <$ (many $ oneOf " \t")
