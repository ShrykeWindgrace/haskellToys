module Parsers.Primitives where

import           Parsers.InlineSpace    (skipSpaces)
import           Text.Megaparsec
import           Text.Megaparsec.String (Parser)

{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> (skipSpaces >> some digitChar) <?> "decimal digit"
