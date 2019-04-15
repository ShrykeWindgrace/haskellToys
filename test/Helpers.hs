module Helpers (parseGen, ParseResult) where


import           Data.Void       (Void)
import           Parsers.Tech    (Parser)
import           Text.Megaparsec


parseGen :: Parser a -> String -> ParseResult a
parseGen _parser = parse _parser ""


type ParseResult a = Either (ParseErrorBundle String Void) a
