module Structures.Header where

newtype Editor = Editor String deriving (Eq, Show)
newtype Title = Title String deriving (Eq, Show)
newtype TDate = TDate String deriving (Eq, Show)