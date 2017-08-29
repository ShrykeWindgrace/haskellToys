module Structures.Words
    -- (
    --     RegWord,
    --     StressedWord,
    --     OneWord
    -- )
    where

newtype RegWord = RegWord String deriving (Eq)

data StressedWord = StressedWord String Char String deriving (Eq)

instance Show RegWord where
    show (RegWord word) = word

instance Show StressedWord where
    show (StressedWord before char after) = before ++ [char, '\x0301'] ++ after

type OneWord = Either StressedWord RegWord
