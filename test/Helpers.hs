module Helpers where

import           Text.Parsec (parse, ParseError)
import           Text.Parsec.String


parseGen :: Parser a -> String -> Either ParseError a
parseGen _parser = parse _parser ""


isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left _)  = True