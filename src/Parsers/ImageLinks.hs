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
-- import           Text.Megaparsec.Perm (makePermParser, (<$$>), (<|?>))
import Control.Applicative.Permutations



sizeParse :: Char -> Parser Integer
sizeParse c = skipSpaces >> char c >>
              skipSpaces >> char '=' >> do
                    s <- skipSpaces >> decimal
                    _ <- skipSpaces >> string "px"
                    return s


widParse :: Parser Integer
widParse = sizeParse 'w'

heiParse :: Parser Integer
heiParse = sizeParse 'h'

linkContents :: Parser ILink
linkContents =  runPermutation $ ILink <$> toPermutation (skipSpaces >>  some (noneOf ")\n") :: Parser String) <*> (toPermutationWithDefault Nothing (toMaybe widParse)) <*> (toPermutationWithDefault Nothing (toMaybe heiParse))
    -- makePermParser $ ILink <$$>
    -- (skipSpaces >>  some (noneOf ")\n") :: Parser String)
    -- <|?> (Nothing,  toMaybe widParse)
    -- <|?> (Nothing,  toMaybe heiParse)


-- (img w=20px h=40px kotik.jpg)


imageLink :: Parser ILink
imageLink =
    between (string "(img")  (char ')') linkContents
