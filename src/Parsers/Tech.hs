module Parsers.Tech (lexeme, toMaybe, wordEnd) where

import           Parsers.InlineSpace (spaces')
import           Text.Parsec
import           Text.Parsec.String


{-|
    Make a parser consume all preceding space.
-}
lexeme :: Parser a -> Parser a
lexeme _parser = spaces' >> _parser


{-|
    Tries to apply the parser; if parser succeeds with value __a__, return 'Just' a.
    If parser fails, backtrack and return 'Nothing'.
-}
toMaybe :: Parser a -> Parser (Maybe a)
-- toMaybe _parser = Just <$> try _parser
toMaybe = fmap Just . try


{-
    Parser for "this is the end of the current word, but we do not consume that end"
-}
wordEnd :: Parser ()
wordEnd = try $ lookAhead $ choice [eof,() <$ space]