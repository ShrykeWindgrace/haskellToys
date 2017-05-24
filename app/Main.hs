module Main where

-- import Lib
-- import StringWorks
import Qs
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)


main :: IO ()
main = --go
  do
    ln <- readFile "input.txt"
    print ln
    let lns = lines ln
    mapM_ someF1 lns -- sequence_ $ map someF1 lns== mapM_ someF1 lns
    mapM_ ( print . length ) lns
    someQQ


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
 

someF1 :: String -> IO ()
someF1 str =  case  parse combiQ "" str of
    Right answ -> print $ "OK: " ++ answ
    Left err -> print err
 

someQQ :: IO ()
someQQ = case parse qGrammar "" "? asd\n\n? fgh" of 
      Right answ -> print $ "Ok: " ++  concat answ
      Left err -> print err


qGrammar :: Parser [String]
qGrammar = do
  s <- many1 qQuest
  eof
  return s



qQuest :: Parser String
qQuest = do
  _ <- char '?'
  spaces 
  s <- many1 qText
  void endOfLine <|> eof
  return $ concat s
  <?> "failed in qQuest"



qText :: Parser String
qText = do
  -- noneOf "?"
  s <- many1 (noneOf "\n") <?> "quest text"
  void (char '\n') <|> eof <?> "line end"
  return $ "QUEST" ++ s
  <?> "failed in qText"