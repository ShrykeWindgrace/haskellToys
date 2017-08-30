module MeltingPot where

import           Control.Monad       (void)
import           Parsers.Inline
import           Parsers.Tokens
import           QNumber
import           Text.Parsec
import           Text.Parsec.String



-- listLines :: Parser String
-- listLines = do
--   _ <- char '-'
--   s <- inlineText
--   nextS <- many listLines
--   return $ concat (("<li> " ++ s ++ "</li>\n") : nextS)

{-|
   Вопрос целиком
-}
fullQuestText :: Parser String
fullQuestText =
  do _ <- many $ char '\n'
     q <- questText
     qm <- questModifier
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
     return $ q ++ a ++ concat opts ++ showQ qm
     <?> "fullQuestText"

{-|
  Тур
-}
tourGrammar :: Parser String
tourGrammar = do
  s <- many1 fullQuestText
  return $ concat s



qSetNumber :: Parser Integer
qSetNumber = do
  _ <- string "№№"
  n <- decimal
  void endOfLine
  return n

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
