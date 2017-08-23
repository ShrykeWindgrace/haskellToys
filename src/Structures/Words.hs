module Structures.Words where

data RegWord = RegWord String deriving (Eq)

instance Show RegWord where
    show (RegWord word) = word