module Parsers.Primitives where

import           Parsers.InlineSpace (skipSpaces)
import           Text.Parsec         (digit, many1, (<?>))
import           Text.Parsec.String  (Parser)

{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> (skipSpaces >> many1 digit) <?> "decimal digit"
