module Helpers where

import           Text.Parsec (parse, ParseError)
import           Text.Parsec.String (Parser)


parseGen :: Parser a -> String -> Either ParseError a
parseGen _parser = parse _parser ""
