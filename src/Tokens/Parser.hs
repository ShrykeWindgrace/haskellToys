module Tokens.Parser where

import Tokens.Tokens
import Parsers.Tech (Parser)
import Text.Megaparsec.Char
--import Text.Megaparsec (anySingle)


parseToken :: Parser Token
parseToken = do
    error "todo"


parseSoftMarker :: Parser Token
parseSoftMarker = parseWithUnbreakable SoftNumber

parseHardMarker :: Parser Token
parseHardMarker = parseWithUnbreakable HardNumber

parseInfoToken :: Parser Token  -- starting with #
parseInfoToken = char '#' >> do
    error "todo"
    

parseWithUnbreakable :: Token -> Parser Token
parseWithUnbreakable t = string (parsingRepresentation t) >> space1 >> pure t