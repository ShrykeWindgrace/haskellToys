module Lib
    ( someFunc
    ) where

import Text.Parsec
import Control.Monad (void)

someFunc :: String -> IO ()
someFunc input =
    print $  parse (balance3 >> eof) "" input


balance3 = void (many brackets) where
  brackets = choice [between (char o) (char c) balance3 |
     (o,c) <- [('(',')'),('[',']'),('{','}')] ]


{-parenSet = char '(' >> many parenSet >> char ')'-}
{-parens = (many parenSet >> eof) <|> eof-}
