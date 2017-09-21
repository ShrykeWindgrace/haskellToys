module Structures.Header where


import Render.StringWorks
newtype Editor = Editor String deriving (Eq, Show)
newtype Title = Title String deriving (Eq, Show)
newtype TDate = TDate String deriving (Eq, Show)

instance ShowNatural Editor where
    showNatural (Editor s) = s

instance ShowNatural Title where
    showNatural (Title s) = s

instance ShowNatural TDate where
    showNatural (TDate s) = s
