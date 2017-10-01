module Parsers.ImageLinks
    (
        imageLink,
        ILink
    )

where

import           Parsers.Primitives     (decimal)
import           Parsers.Tech           (lexeme, toMaybe)
import           Structures.Words       (ILink (..))
import           Text.Megaparsec        (between, char, noneOf, some, string)
import           Text.Megaparsec.Perm   (makePermParser, (<$$>), (<|?>))
import           Text.Megaparsec.String (Parser)



sizeParse :: Char -> Parser Integer
sizeParse c = do
    _ <- lexeme $ char c
    _ <- lexeme $ char '='
    s <- lexeme decimal
    _ <- lexeme $ string "px"
    return s


widParse :: Parser Integer
widParse = sizeParse 'w'

heiParse :: Parser Integer
heiParse = sizeParse 'h'

linkContents :: Parser ILink
linkContents =  makePermParser (ILink
    <$$>  lexeme (some (noneOf ")\n"))
    <|?> (Nothing,  toMaybe widParse)
    <|?> (Nothing,  toMaybe heiParse))


-- (img w=20px h=40px kotik.jpg)


imageLink :: Parser ILink
imageLink =
    between (string "(img")  (char ')') linkContents
