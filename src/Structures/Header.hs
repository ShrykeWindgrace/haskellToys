{-# LANGUAGE OverloadedStrings #-}
module Structures.Header where


import           Constants.StringWorks (Element4s, cssClass, dateLine, edLine,
                                        parsingToken, showNatural, titleLine,
                                        tokenToString)
import Structures.Lines (Line)
import           Data.Text             (append, pack)


data HeaderItemType = Editor | Title | TDate deriving (Eq, Show, Enum)

allHeaderItemTypes :: [HeaderItemType]
allHeaderItemTypes = [toEnum 0 ..]

data HeaderItem = HeaderItem HeaderItemType Line deriving (Eq, Show)

instance Element4s HeaderItemType where
    parsingToken Editor = edLine
    parsingToken Title  = titleLine
    parsingToken TDate  = dateLine

    showNatural  = tokenToString . parsingToken

    cssClass = pack . show --let's be lazy here


instance Element4s HeaderItem where
    showNatural (HeaderItem _ s) = show s
    parsingToken (HeaderItem t _) = parsingToken t
    cssClass (HeaderItem t _) = cssClass t `append` "text"
