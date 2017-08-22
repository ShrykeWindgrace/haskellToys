module Tech (lexeme) where

import Text.Parsec
import Text.Parsec.String
import InlineSpace (spaces')

lexeme :: Parser a -> Parser a
lexeme _parser = spaces' >> _parser

