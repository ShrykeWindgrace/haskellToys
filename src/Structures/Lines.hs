module Structures.Lines where

import           Structures.Words (OneWord)
-- import           Constants.StringWorks

data Line = Line [OneWord] | ListLinesStr ListLines deriving (Eq, Show)

-- instance Show Line where
    -- show (Line _words) = unwords (show <$> _words)

-- instance Monoid Line where
--     mempty = Line []
--     Line lst `mappend` Line lst2 = Line $ lst ++ lst2
    -- this instance supposes that these lines are not separated; there are no newlines here


newtype ListLines = ListLines [Line] deriving (Eq, Show)
