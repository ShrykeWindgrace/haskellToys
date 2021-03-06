module Parsers.Tech where

{-import           Parsers.InlineSpace    (skipSpaces)-}
-- import           Parsers.Types
-- import           Text.Parsec         (choice, eof, lookAhead, space, try)
-- import           Text.Parsec.String  (Parser)
import           Text.Megaparsec (Parsec, try)
-- import           Text.Megaparsec.String (Parser)
import Data.Void (Void)
import Control.Applicative (optional)



{-|
    Make a parser consume all preceding space.
-}
{-lexeme :: Parser a -> Parser a-}
{-lexeme _parser = skipSpaces >> _parser-}


{-|
    Tries to apply the parser; if parser succeeds with value __a__, return 'Just' a.
    If parser fails, backtrack and return 'Nothing'.
-}
toMaybe :: Parser a -> Parser (Maybe a)
-- toMaybe _parser = Just <$> try _parser
-- toMaybe = fmap Just . try
toMaybe = optional . try


type Parser = Parsec Void String

