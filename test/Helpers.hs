module Helpers (parseGen, ParseResult) where


import           Data.Void       (Void)
import           Parsers.Tech    (Parser)
import           Text.Megaparsec (ParseError, parse)


parseGen :: Parser a -> String -> ParseResult a
parseGen _parser = parse _parser ""


type ParseResult a = Either (ParseError Char Void) a
