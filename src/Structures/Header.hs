module Structures.Header where


import           Constants.StringWorks


data HeaderItemType = Editor | Title | TDate deriving (Eq, Show, Enum)

allHeaderItemTypes :: [HeaderItemType]
allHeaderItemTypes = [toEnum 0 ..]

data HeaderItem = HeaderItem HeaderItemType String deriving (Eq, Show)
instance ShowNatural HeaderItemType where
    showNatural  = tokenToString . tokenOf

instance ShowNatural HeaderItem where
    showNatural (HeaderItem _ s) = s


instance HasToken HeaderItemType where
    tokenOf Editor = edLine
    tokenOf Title  = titleLine
    tokenOf TDate  = dateLine
