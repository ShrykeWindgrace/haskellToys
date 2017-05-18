module StringWorks

(
printField
)

where

printField :: Maybe String -> String
printField Maybe s = "<p>" ++ s ++ "</p>"
printField Nothing = ""
