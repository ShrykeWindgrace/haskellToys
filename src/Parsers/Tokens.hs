module Parsers.Tokens (
          equivText
         ,  authorText
         ,  sourceText
         ,  commentText
         ,  notEquivText
         ,  listLines
         ,  questText
         ,  answerText
         ,  tournamentHeader
         ,  editorHeader
  ) where

import           Parsers.InlineSpace
import           Render.StringWorks
import           Text.Megaparsec        
import           Text.Megaparsec.String

inlineText = undefined
rawText = undefined
listLines = undefined

{-|
  List of recognised tokens as well as an endOfLine symbol

  regular lines can not begin with these symbols
-}
tokenList :: String
tokenList = "@?!-\n№#/=^<>["


-- |
--   parser for single-character tokens
tokenLines :: Char -> Parser String
tokenLines c =
  do _ <- char c
     spaces'
     _ <- optional newline
     ss <- some (try inlineText <|> listLines)
     return $ "(" ++ tokenToString c ++ ")" ++ concat ss
     <?> ("line with token " ++ [c])

-- |
--   parser for multicharacter tokens
longTokenLines :: String -> Parser String
longTokenLines str =
  do _ <- string str
     ss <- some $ try rawText
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
