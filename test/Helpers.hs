module Helpers (parseGen, ParseResult) where

import           Text.Megaparsec        (Dec, ParseError, Token, parse)
import           Text.Megaparsec.String (Parser)


parseGen :: Parser a -> String -> ParseResult a
parseGen _parser = parse _parser ""


type ParseResult a = Either (ParseError (Token String) Dec) a
