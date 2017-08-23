module Render.StringWorks (longTokenToString, tokenToString)

where

showMaybe :: Maybe String -> String
showMaybe (Just s) = s
showMaybe Nothing  = ""

tokenToString :: Char -> String
tokenToString '?' = "Вопрос"
tokenToString '=' = "Зачёт"
tokenToString '@' = "Автор(ы)"
tokenToString '^' = "Источник(и)"
tokenToString '!' = "Ответ"
tokenToString '/' = "Комментарии"
tokenToString _   = "Неизвестный токен"


longTokenToString :: String -> String
longTokenToString "###"     = "Название турнира"
longTokenToString "#EDITOR" = "Редактор"
longTokenToString "#DATE"   = "Время и место проведения"
longTokenToString "!="      = "незачёт"
longTokenToString _         = "Неизвестный токен"
