module Main where

import Lib
import StringWorks

main :: IO ()
main = go
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
 
