{-# LANGUAGE OverloadedStrings #-}
{-# language StrictData #-}

module Structures.Header where


import           Constants.StringWorks (Element4s, cssClass, dateLine, edLine,
                                        parsingToken, showNatural, titleLine,
                                        tokenToString)
import           Data.Text             (append, pack)
import           Structures.Lines      (Line)


data HeaderItemType = Editor | Title | TDate deriving (Eq, Show, Enum, Bounded)

allHeaderItemTypes :: [HeaderItemType]
allHeaderItemTypes = [minBound .. maxBound]

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
