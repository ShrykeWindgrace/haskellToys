module Structures.Words
    -- (
    --     RegWord,
    --     StressedWord,
    --     OneWord
    -- )
    where

data OneWord = StressedWord String Char String | RegWord String
    deriving (Eq)

instance Show OneWord where
    show (RegWord word) = word
    show (StressedWord before char after) = before ++ [char, '\x0301'] ++ after
    -- show _ = undefined