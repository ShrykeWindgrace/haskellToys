module MeltingPot where

import           Control.Monad       (void)
import           Text.Parsec
import           Text.Parsec.String

testGrammar :: Parser String
testGrammar = undefined

someQQ :: Bool -> String -> String -> IO ()
someQQ cons inF outF = do
  parseResult <- parseFromFile testGrammar inF
  case parseResult of
    Right answ ->
      putStrLn
        ("Ok! " ++
         if cons
           then answ
           else "") >>
      writeFile outF answ
    Left err -> print err
