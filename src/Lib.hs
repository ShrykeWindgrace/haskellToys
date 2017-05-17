module Lib
    ( someFunc
    ) where

import Text.Parsec

someFunc :: IO ()
someFunc =
    if Right () == parse parens "" "()"
    then 
        putStrLn "parsed"
    else
        putStrLn "not parsed"


parenSet = char '(' >> many parenSet >> char ')'
parens = (many parenSet >> eof) <|> eof
