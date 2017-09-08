module Parsers.ImageLinks
    -- (
    --     imageLink,
    --     imageLink',
    --     ILink
    -- )

where

import           Parsers.Inline             (decimal)
import           Structures.Words
import           Parsers.Tech               (lexeme, toMaybe)
import           Text.Megaparsec        
import           Text.Megaparsec.Perm
import           Text.Megaparsec.String



sizeParse :: Char -> Parser Integer
sizeParse c = do
    lexeme $ char c
    lexeme $ char '='
    s <- lexeme decimal
    lexeme $ string "px"
    return s


widParse :: Parser Integer
widParse = sizeParse 'w'

heiParse :: Parser Integer
heiParse = sizeParse 'h'

linkContents :: Parser ILink
linkContents =  makePermParser (ILink
    <$$> lexeme ( some (noneOf ")\n"))
    <|?> (Nothing,  toMaybe widParse)
    <|?> (Nothing,  toMaybe heiParse))


-- (img w=20px h=40px kotik.jpg)


imageLink :: Parser ILink
imageLink =
    between (string "(img")  (char ')') linkContents
