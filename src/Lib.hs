module Lib
    ( someFunc
    ) where

import Text.Parsec
-- import Text.Parsec.String
import Control.Monad (void)
import Data.Functor.Identity

someFunc :: String -> IO ()
someFunc input =
    print $  parse (balance3 >> eof) "" input

balance3 :: ParsecT String u Data.Functor.Identity.Identity ()
balance3 = void (many brackets) where
  brackets = choice $  noise : [between (char o) (char c) balance3 |
     (o,c) <- [('(',')'),('[',']'),('{','}')] ]


{-parenSet = char '(' >> many parenSet >> char ')'-}
{-parens = (many parenSet >> eof) <|> eof-}

noise :: ParsecT String u Data.Functor.Identity.Identity ()
noise = void $ many1 alphaNum
