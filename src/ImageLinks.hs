module ImageLinks
    -- (
    --     imageLink,
    --     imageLink',
    --     ILink
    -- )

where

import           Inline             (decimal)
import           Tech             (lexeme)
import           InlineSpace        (spaces')
import           Text.Parsec
import           Text.Parsec.Perm
import           Text.Parsec.String



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


linkContents :: Parser ()
linkContents = do
    spaces'
    optional widParse
    spaces'
    optional heiParse
    spaces'
    () <$ many1 (noneOf ")\n")


linkContents' :: Parser ILink
linkContents' = do
    spaces'
    w <- optionMaybe widParse
    spaces'
    h <- optionMaybe heiParse
    spaces'
    l <- many1 $ noneOf ")\n"
    return $ ILink l w h



toMaybe :: Parser a -> Parser (Maybe a)
-- toMaybe _parser = Just <$> try _parser
toMaybe = fmap Just . try


linkContentsP :: Parser ILink
linkContentsP =  permute (ILink
    <$$> (spaces' >> many1 (noneOf ")\n"))
    <|?> (Nothing,  toMaybe widParse)
    <|?> (Nothing,  toMaybe heiParse))


-- (img w=20px h=40px kotik.jpg)
imageLink :: Parser Char
imageLink = do
    spaces'
    between (string "(img")  (char ')') linkContents
    return '&'


imageLink' :: Parser ILink
imageLink' =
    between (string "(img")  (char ')') linkContents'

imageLinkP :: Parser ILink
imageLinkP =
    between (string "(img")  (char ')') linkContentsP



data ILink = ILink {
    link   :: String,
    width  :: Maybe Integer,
    height :: Maybe Integer
    } deriving (Eq, Show)
