module Main where

-- import Lib
-- import StringWorks
-- import Qs
import Q2
-- import Text.Parsec
-- import Text.Parsec.String
-- import Control.Monad (void)
import OptionsCLI
-- import Data.Semigroup ((<>))



main :: IO ()
main = --go
       main' =<< execParser optionsH
         -- where
           
  -- do
    -- ln <- readFile "input.txt"
    -- print ln
    -- let lns = lines ln
    -- mapM_ someF1 lns -- sequence_ $ map someF1 lns== mapM_ someF1 lns
    -- mapM_ ( print . length ) lns
    -- someQQ2
    -- someQQ3

main' :: Options -> IO()
main' opt = 
  if dryRun opt
   then putStrLn "Dry Run"
   else putStrLn (input opt) >> putStrLn (output opt) >> someQQ3 (input opt) (output opt)



-- go :: IO ()
-- go = do
--   s <- getLine
--   if s == "q"
--   then
--     putStrLn "Bye"
--   else
--     do
--       someF1 s
--       go
 

-- someF1 :: String -> IO ()
-- someF1 str =  case  parse combiQ "" str of
--     Right answ -> print $ "OK: " ++ answ
--     Left err -> print err
 
