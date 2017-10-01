module Helpers where

import           Text.Megaparsec        (Dec, ParseError, Token, parse)
import           Text.Megaparsec.String (Parser)


parseGen :: Parser a -> String -> Either (ParseError (Token String) Dec) a
parseGen _parser = parse _parser ""
