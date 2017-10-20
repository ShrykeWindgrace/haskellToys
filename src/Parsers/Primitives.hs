module Parsers.Primitives (decimal) where

import           Parsers.InlineSpace    (skipSpaces)
import           Text.Megaparsec ((<?>), some)
import           Text.Megaparsec.Char (digitChar)
import           Parsers.Tech (Parser)

{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> (skipSpaces >> some digitChar) <?> "decimal digit"
