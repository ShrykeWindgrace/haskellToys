module StringWorks

(
printField
)

where

printField :: Maybe String -> String
printField (Just s) = "<p>" ++ s ++ "</p>"
printField Nothing = ""
