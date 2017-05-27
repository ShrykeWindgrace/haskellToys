module AST where

data QText = QText {
    runText :: String,
    runAnswer :: String
} deriving (Eq, Show)


printQS:: [QText] -> String
printQS [] = ""
printQS quests = printQS' 1 quests where
    printQS' :: Int -> [QText] -> String
    printQS' _ [] = ""
    printQS' n (q:qs) = "Question " ++ show n ++ "."
        ++ runText q ++ "\r\n"
        ++ "Answer: " ++ runAnswer q
        ++ printQS' (n+1) qs