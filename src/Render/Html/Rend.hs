{-# LANGUAGE OverloadedStrings #-}

module Render.Html.Rend where 


import Structures.QNumber
import Structures.Words
import Data.Text
import Lucid
import Data.Maybe (maybeToList)


instance ToHtml QModifier where
    toHtml (Soft line) = div_ [class_ "qnm"] $ do
            div_ [class_ "questWord"] "Вопрос "
            div_ [class_ "questNumber"] $ toHtml line -- line is escaped

    toHtml (Hard int) = div_ [class_ "qnm"] $ do
            div_ [class_ "questWord"] "Вопрос "
            div_ [class_ "questNumber"] $ toHtml $ show int

    toHtmlRaw (Soft line) = div_ [class_ "qnm"] $ do
            div_ [class_ "questWord"] "Вопрос "
            div_ [class_ "questNumber"] $ toHtmlRaw line -- line is not escaped

    toHtmlRaw (Hard int) = div_ [class_ "qnm"] $ do
            div_ [class_ "questWord"] "Вопрос "
            div_ [class_ "questNumber"] $ toHtmlRaw $ show int


instance ToHtml OneWord where
    toHtml (RegWord str) = toHtml str
    toHtml a@StressedWord{} = toHtml (show a)
    toHtml (ILinkStr iLink) = img_ $ [src_ $ pack $ link iLink] ++
        ( width_ . pack . show <$> maybeToList (width iLink) ) ++
        ( width_ . pack . show <$> maybeToList (height iLink) )


    toHtmlRaw (RegWord str) = toHtmlRaw str
    toHtmlRaw a@StressedWord{} = toHtmlRaw (show a)
    toHtmlRaw  (ILinkStr iLink) = img_ $ [src_ $ pack $ link iLink] ++
        ( width_ . pack . show <$> maybeToList (width iLink) ) ++
        ( width_ . pack . show <$> maybeToList (height iLink) )
