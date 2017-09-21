module Structures.Header where


import Render.StringWorks


data HeaderItem = Editor String | Title String | TDate String deriving (Eq, Show)

instance ShowNatural HeaderItem where
    showNatural (Editor s) = s
    showNatural (Title s) = s
    showNatural (TDate s) = s

instance HasToken HeaderItem where
    tokenOf (Editor _) = edLine
    tokenOf _ = undefined