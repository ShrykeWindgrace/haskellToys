module StringWorks

where

showMaybe :: Maybe String -> String
showMaybe (Just s) = s
showMaybe Nothing = ""

tokenToString :: Char -> String
tokenToString '?' = "Вопрос"
tokenToString '=' = "Зачёт"
tokenToString '@' = "Автор(ы)"
tokenToString '^' = "Источник(и)"
tokenToString '!' = "Ответ"
tokenToString '/' = "Комментарии"
tokenToString _ = "unknown token"