module Main where

import Lib
import StringWorks

main :: IO ()
main = do
  someFunc
  putStrLn $ printField $ Just "ok"
