module AST (
    QText,
    printQS)
 where

data QText = QText {
    runText :: String,
    runAnswer :: String
} deriving (Eq, Show)


printQS:: [QText] -> String
printQS [] = ""
printQS qs = printQS' 1 qs where
    printQS' n [] = ""
    printQS' n (q:qs) = "Question " ++ show n ++ "." ++ runText q ++ "\r\n" ++ "Answer: " ++ runAnswer q ++ (printQS' (n+1) qs)