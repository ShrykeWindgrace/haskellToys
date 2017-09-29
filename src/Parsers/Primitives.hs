module Parsers.Primitives where

import           Parsers.InlineSpace
import           Text.Parsec
import           Text.Parsec.String

{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> (skipSpaces >> many1 digit) <?> "decimal digit"
