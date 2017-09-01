module Structures.Lines where 

import Structures.Words

data Line = Line [OneWord] deriving (Eq)

instance Show Line where
    show (Line words) = unwords (show <$> words)

