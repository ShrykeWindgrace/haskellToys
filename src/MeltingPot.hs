module MeltingPot where

import           Control.Monad       (void)
import           Parsers.Tokens
import           Text.Parsec
import           Text.Parsec.String



{-|
   Вопрос целиком
-}
fullQuestText :: Parser String
fullQuestText =
  do _ <- many $ char '\n'
     q <- questText
     -- qm <- questModifier
     a <- answerText
     opts <-
       many $
       choice
         [ try equivText
         , try authorText
         , try sourceText
         , try commentText
         , try notEquivText
         , try listLines
         ]
  -- li <- optionMaybe listLines
  -- a'<- optionMaybe equivText
     void endOfLine <|> eof
     return $ q ++ a ++ concat opts -- ++ showQ qm
     <?> "fullQuestText"

{-|
  Тур
-}
tourGrammar :: Parser String
tourGrammar = do
  s <- many1 fullQuestText
  return $ concat s



testGrammar :: Parser String
testGrammar = do
  to <- tournamentHeader
  spaces
  ed <- editorHeader
  spaces
  tour <- tourGrammar
  return $ to ++ ed ++ tour



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
