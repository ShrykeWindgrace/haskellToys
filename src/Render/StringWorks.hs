module Render.StringWorks

where

showMaybe :: Maybe String -> String
showMaybe (Just s) = s
showMaybe Nothing  = ""

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

class ShowNatural a where
    showNatural :: a -> String

class HasToken a where
    tokenOf :: a -> String
    -- showUnnatural :: a -> String
