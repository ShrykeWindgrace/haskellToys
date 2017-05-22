module Main where

import Lib
import StringWorks
import Qs
import Text.Parsec
main :: IO ()
main = --go
  case  parse parseQ "" "? asd ert?" of
    Right answ -> print $ "OK: " ++ answ
    Left err -> print err
  {-putStrLn $ printField $ Just "ok"-}


go :: IO ()
go = do
  s <- getLine
  if s == "q"
  then
    putStrLn "Bye"
  else
    do
      someFunc s
      go
 
