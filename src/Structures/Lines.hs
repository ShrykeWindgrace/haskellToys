module Structures.Lines where

import           Structures.Words

data Line = Line [OneWord] deriving (Eq, Show) -- TODO rename to avoid "qualified imports" requirement

-- instance Show Line where
    -- show (Line _words) = unwords (show <$> _words)


data ListLines = ListLines [Line] deriving (Eq, Show)
