module Structures.Header where


import           Constants.StringWorks


data HeaderItemType = Editor | Title | TDate deriving (Eq, Show, Enum)

allHeaderItemTypes :: [HeaderItemType]
allHeaderItemTypes = [toEnum 0 ..]

data HeaderItem = HeaderItem HeaderItemType String deriving (Eq, Show)

instance Element4s HeaderItemType where
    parsingToken Editor = edLine
    parsingToken Title  = titleLine
    parsingToken TDate  = dateLine
    
    showNatural  = tokenToString . parsingToken

    cssClass = show --let's be lazy here


instance Element4s HeaderItem where
    showNatural (HeaderItem _ s) = s
    parsingToken (HeaderItem t _) = parsingToken t
    cssClass (HeaderItem t _) = (++ "text") (cssClass t)
