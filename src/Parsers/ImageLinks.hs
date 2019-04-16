module Parsers.ImageLinks
    (
        imageLink,
        ILink
    )

where

import           Parsers.InlineSpace  (skipSpaces)
import           Parsers.Primitives   (decimal)
import           Parsers.Tech         (toMaybe, Parser)
import           Structures.Words     (ILink (..))
import           Text.Megaparsec      (between, noneOf, some)
import           Text.Megaparsec.Char (char,  string)


sizeParse :: Char -> Parser Integer
sizeParse c = skipSpaces >> char c >>
              skipSpaces >>
                    between (char '=') (skipSpaces >> string "px") decimal
                    

widParse :: Parser Integer
widParse = sizeParse 'w'

heiParse :: Parser Integer
heiParse = sizeParse 'h'


-- (img w=20px h=40px kotik.jpg)

linkContents :: Parser ILink
linkContents = do
    skipSpaces
    (w,h) <- tryCombine widParse heiParse
    name <- skipSpaces >>  some (noneOf ")\n")
    pure $ ILink name w h
    


imageLink :: Parser ILink
imageLink = between (string "(img")  (char ')') linkContents


-- ugly hack, there should be a better way to do it
tryCombine :: Parser b -> Parser c -> Parser (Maybe b, Maybe c)
tryCombine b c = do  
    b1 <- toMaybe b
    case b1 of
        Just b2 -> do
            c1 <- toMaybe c
            pure (Just b2, c1)
        Nothing -> do
            c1 <- toMaybe c
            case c1 of
                Just c2 -> do
                    b3 <- toMaybe b
                    pure (b3, Just c2)
                Nothing -> pure (Nothing, Nothing)
