module Structures.Header where


import           Render.StringWorks

data HeaderItemType = Editor | Title | TDate deriving (Eq, Show)

data HeaderItem = HeaderItem HeaderItemType String deriving (Eq, Show)

instance ShowNatural HeaderItem where
    showNatural (HeaderItem _ s) = s
    -- showNatural Title s)  = s
    -- showNatural TDate s)  = s

instance HasToken HeaderItemType where
    tokenOf Editor = edLine
    tokenOf Title  = titleLine
    tokenOf TDate  = dateLine
    tokenOf _      = error "invalid header item type encountered"
