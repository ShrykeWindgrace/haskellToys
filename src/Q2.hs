module Q2 where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)


import StringWorks 
import ImageLinks 

{-|
  List of recognised tokens as well as an endOfLine symbol

  regular lines can not begin with these symbols
-}
tokenList :: String
tokenList = "@?!-\n№#/=^"


{-|
  Current implementation of regular text
  Can not start from a token or endOfLine

  __TODO:__

  implement inline modifiers:
      
      * emphasis
      * image links
      * accents
-}
inlineText :: Parser String
inlineText = do
  c <- noneOf tokenList
  s <- many1 $ try imageLink <|>  noneOf "\n"
  void (char '\n') <|> eof 
  return (c:s++"\n")
  <?> "inlineText"

-- | do not parse anything inside this string
rawText :: Parser String 
rawText = do
  c <- noneOf tokenList
  s <- many1 $  noneOf "\n"
  void (char '\n') <|> eof 
  return (c:s++"\n")
  <?> "rawText"

-- |
--   parser for single-character tokens
tokenLines :: Char -> Parser String
tokenLines c = do
  _ <- char c
  _ <- optionMaybe $ char '\n'
  ss <- many1 $ try inlineText
  return $ "(" ++ tokenToString c ++ ")" ++ concat ss
  <?> ("line with token " ++ [c])


-- |
--   parser for multicharacter tokens
longTokenLines :: String -> Parser String
longTokenLines str = do
  _ <- string str
  ss <- many1 $ try rawText
  return $ '(' : longTokenToString str ++ ")" ++ concat ss
  <?> ("line with long token " ++ str)


{-|
  Текст вопроса
-}
questText :: Parser String
questText = tokenLines '?'


{-|
  Текст ответа
-}
answerText :: Parser String
answerText = tokenLines '!'


{-|
  Текст зачёта
-}
equivText :: Parser String
equivText = tokenLines '='


{-|
  Текст "незачёта"
-}
notEquivText :: Parser String
notEquivText = longTokenLines "!="


{-|
  Автор(ы)
-}
authorText :: Parser String
authorText = tokenLines '@'

{-|
  Текст комментариев
-}
commentText :: Parser String
commentText = tokenLines '/'

{-|
  Источник
-}
sourceText :: Parser String
sourceText = tokenLines '^'

{-|
   Вопрос целиком
-}
fullQuestText :: Parser String
fullQuestText = do
  _ <- many $ char '\n'
  q <- questText
  a <- answerText
  a'<- optionMaybe equivText
  void endOfLine <|> eof
  return $ q ++ a ++ showMaybe a'
  <?> "fullQuestText"



{-|
  Тур
-}
tourGrammar :: Parser String
tourGrammar = do
  s <- many1 fullQuestText
  return $ concat s 


{-|
  Редактор
-}
editorHeader :: Parser String
editorHeader = longTokenLines "#EDITOR" 


{-|
  Дата и место проведения
-}
dateHeader :: Parser String
dateHeader = longTokenLines "#DATE" 


{-|
  Название турнира
-}
tournamentHeader :: Parser String
tournamentHeader = longTokenLines "###" 


qSetNumber :: Parser Integer
qSetNumber = do
  _ <- string "№№"
  spaces
  n <- many1 digit
  void endOfLine
  return $ read n

testGrammar :: Parser String
testGrammar = do
  ed <- editorHeader
  _ <- endOfLine
  tour <- tourGrammar
  return $ ed ++ tour



blankLine :: Parser String
blankLine = do
  _ <- many $ char ' '
  _ <- char '\n'
  return ""
  <?> "blankLine"


someQQ2 :: IO()
someQQ2 = case parse tourGrammar "" "? q1\n! a1\n\n? q2 \n! a2" of 
      Right answ -> print $ "Ok: " ++   answ
      Left err -> print err

someQQ3 :: IO ()
someQQ3 = do
  file <- readFile "input2.txt"
  case parse testGrammar "" file of 
      Right answ -> print  ("Ok: " ++   answ) >>
        writeFile "out2.txt" answ
      Left err -> print err
