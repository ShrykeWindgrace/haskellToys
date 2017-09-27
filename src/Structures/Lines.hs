module Structures.Lines where

import           Structures.Words
import           Constants.StringWorks

data Line = Line [OneWord] deriving (Eq, Show) -- TODO rename to avoid "qualified imports" requirement

-- instance Show Line where
    -- show (Line _words) = unwords (show <$> _words)

instance Monoid Line where
    mempty = Line []
    Line lst `mappend` Line lst2 = Line $ lst ++ lst2
    -- this instance supposes that these lines are not separated; there are no newlines here

instance ShowNatural Line where 
    showNatural (Line _words) = unwords (show <$> _words)

data ListLines = ListLines [Line] deriving (Eq, Show)
