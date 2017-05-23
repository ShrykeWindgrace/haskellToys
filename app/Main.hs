module Main where

import Lib
import StringWorks
import Qs
import Text.Parsec
main :: IO ()
main = --go
  do
    ln <- readFile "input.txt"
    print ln
    let lns = lines ln
    mapM_ someF1 lns -- sequence_ $ map someF1 lns== mapM_ someF1 lns
    mapM_ ( print . length ) lns
 {-putStrLn $ printField $ Just "ok"-}


go :: IO ()
go = do
  s <- getLine
  if s == "q"
  then
    putStrLn "Bye"
  else
    do
      someF1 s
      go
 


someF1 str =  case  parse combiQ "" str of
    Right answ -> print $ "OK: " ++ answ
    Left err -> print err
 
qGrammar = many 
