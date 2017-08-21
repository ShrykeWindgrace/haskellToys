module Helpers where

import           Text.Parsec (parse, ParseError)
import           Text.Parsec.String


parseGen :: Parser a -> String -> Either ParseError a
parseGen _parser = parse _parser ""
