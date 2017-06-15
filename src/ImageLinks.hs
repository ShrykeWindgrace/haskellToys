module ImageLinks
    (
        imageLink
    )

where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import InlineSpace (spaces')


sizeParse :: Char -> Parser Integer
sizeParse c = do
    void $ char c
    spaces'
    void $ char '='
    spaces'
    s <- many1 digit
    void $ string "px"
    return $ read s


widParse :: Parser Integer
widParse = sizeParse 'w'

heiParse :: Parser Integer
heiParse = sizeParse 'h'


linkContents :: Parser ()
linkContents = do
    spaces'
    optional widParse
    spaces'
    optional heiParse
    spaces'
    void $ many1 $ noneOf ")\n"


-- (img w=20px h=40px kotik.jpg)
imageLink :: Parser Char 
imageLink = do
    between (string "(img")  (char ')') linkContents
    return '&'
