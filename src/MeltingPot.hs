module MeltingPot where

import           Control.Monad      (void)
import           Text.Parsec
import           Text.Parsec.String

import           ImageLinks
import           Inline
import           InlineSpace
import           QNumber
import           Tech               (lexeme)
import           Render.StringWorks

{-|
  List of recognised tokens as well as an endOfLine symbol

  regular lines can not begin with these symbols
-}
tokenList :: String
tokenList = "@?!-\n№#/=^<>"

{-|
  Current implementation of regular text
  Can not start from a token or endOfLine

  __TODO:__

  implement inline modifiers:

      * emphasis
-}
inlineText :: Parser String
inlineText =
  do c <- noneOf tokenList
     s <- many1 $ (const '&' <$> try imageLink) <|> try emphText'' <|> noneOf "\n"
     void (char '\n') <|> eof
     return (c : s ++ "\n")
     <?> "inlineText"

-- | do not parse anything inside this string
rawText :: Parser String
rawText =
  do c <- noneOf tokenList
     s <- many1 $ noneOf "\n"
     void (char '\n') <|> eof
     return (c : s ++ "\n")
     <?> "rawText"

-- |
--   parser for single-character tokens
tokenLines :: Char -> Parser String
tokenLines c =
  do _ <- char c
     spaces'
     _ <- optionMaybe $ char '\n'
     ss <- many1 (try inlineText <|> listLines)
     return $ "(" ++ tokenToString c ++ ")" ++ concat ss
     <?> ("line with token " ++ [c])

-- |
--   parser for multicharacter tokens
longTokenLines :: String -> Parser String
longTokenLines str =
  do _ <- string str
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

listLines :: Parser String
listLines = do
  _ <- char '-'
  s <- inlineText
  nextS <- many listLines
  return $ concat (("<li> " ++ s ++ "</li>\n") : nextS)

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

blankLine :: Parser ()
blankLine = void (lexeme endOfLine <?> "\"\\n\" or \"\\r\\n\"")

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
