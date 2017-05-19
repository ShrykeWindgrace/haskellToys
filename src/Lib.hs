module Lib
    ( someFunc
    ) where

import Text.Parsec

someFunc :: String -> IO ()
someFunc str=
    if Right () == parse parens "" str
    then 
        putStrLn "parsed"
    else
        putStrLn "not parsed"


parenSet = char '(' >> many parenSet >> char ')'
parens = (many parenSet >> eof) <|> eof
