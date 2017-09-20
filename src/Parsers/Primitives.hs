module Parsers.Primitives where

import           Parsers.Tech       (lexeme)
import           Text.Parsec
import           Text.Parsec.String

{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> lexeme (many1 digit) <?> "decimal digit"
