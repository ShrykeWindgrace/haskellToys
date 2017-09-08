module MeltingPot where

import           Control.Monad          (void)
import           Parsers.InlineSpace    (spaces')
import           Parsers.Tokens
import           Text.Megaparsec
import           Text.Megaparsec.String



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
     void eol <|> eof
     return $ q ++ a ++ concat opts -- ++ showQ qm
     <?> "fullQuestText"

{-|
  Тур
-}
tourGrammar :: Parser String
tourGrammar = do
  s <- some fullQuestText
  return $ concat s



testGrammar :: Parser String
testGrammar = do
  to <- tournamentHeader
  spaces'
  ed <- editorHeader
  spaces'
  tour <- tourGrammar
  return $ to ++ ed ++ tour
