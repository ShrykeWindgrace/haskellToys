module Constants.StringWorks

where

import           Data.Maybe (fromMaybe)
import           Data.Text  (Text)

showMaybe :: Maybe String -> String
showMaybe = fromMaybe ""

tokenToString :: String -> String
tokenToString "?"       = "Вопрос"
tokenToString "="       = "Зачёт"
tokenToString "@"       = "Автор(ы)"
tokenToString "^"       = "Источник(и)"
tokenToString "!"       = "Ответ"
tokenToString "/"       = "Комментарии"
tokenToString "###"     = "Название турнира"
tokenToString "#EDITOR" = "Редактор"
tokenToString "#DATE"   = "Время и место проведения"
tokenToString "!="      = "незачёт"
tokenToString _         = "Неизвестный токен"

edLine :: String
edLine = "#EDITOR"

dateLine :: String
dateLine = "#DATE"

titleLine :: String
titleLine = "###"


class Element4s a where
    parsingToken :: a -> String -- the corresponding token in *.4s
    showNatural :: a -> String  -- the token shown in rendering
    cssClass :: a -> Text --the corresponding css class


{-|
  Regular lines can not begin with these symbols
-}

tokenList :: String
tokenList = "@?!-\n№#/=^<>"
