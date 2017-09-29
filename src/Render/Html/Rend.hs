{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Render.Html.Rend where


import           Constants.StringWorks
import           Data.List             (sort)
import           Data.Maybe            (maybeToList)
import           Data.Text             hiding (foldr1, map)
import           Lucid
import           Structures.Header
import           Structures.Lines
import           Structures.QNumber
import           Structures.Quest
import           Structures.Words

instance ToHtml QModifier where
    toHtml a@(Soft line) = div_ [class_ $ cssClass a] $ do
            div_ [class_ "questWord"] $ toHtml $ showNatural a
            div_ [class_ "questNumber"] $ toHtml line -- line is escaped

    toHtml a@(Hard int) = div_ [class_ $ cssClass a] $ do
            div_ [class_ "questWord"] $ toHtml $ showNatural a
            div_ [class_ "questNumber"] $ toHtml $ show int

    toHtmlRaw a@(Soft line) = div_ [class_ $ cssClass a] $ do
            div_ [class_ "questWord"] $ toHtmlRaw $ showNatural a
            div_ [class_ "questNumber"] $ toHtmlRaw line -- line is escaped

    toHtmlRaw a@(Hard int) = div_ [class_ $ cssClass a] $ do
            div_ [class_ "questWord"] $ toHtmlRaw $ showNatural a
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


instance ToHtml Line where
    toHtml (Line list) = div_ $ htmlListFold list

    toHtmlRaw (Line list) = div_ $ htmlListFoldRaw list


instance ToHtml Question where
    toHtml Question {..} = div_ [class_ "question"] $ do
        toHtml modifier
        let sorted_ = sort fields
        htmlListFold sorted_
    toHtmlRaw  Question {..} = div_ [class_ "question"] $ do
        toHtml modifier
        let sorted_ = sort fields
        htmlListFoldRaw sorted_

instance ToHtml QField where
    toHtml (QField t list) = div_ [class_ $ cssClass t] $ htmlListFold list

    toHtmlRaw (QField t list) = div_ [class_ $ cssClass t] $ htmlListFoldRaw list


htmlListFold :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFold = htmlListFoldBase toHtml

htmlListFoldRaw :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFoldRaw = htmlListFoldBase toHtmlRaw

htmlListFoldBase :: (ToHtml a, Monad m) => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBase fn = foldr1 mappend . map fn


instance ToHtml HeaderItem where
    toHtml (HeaderItem t str)
        | t == Title = h1_ $ toHtml $ pack str
        | otherwise = div_ [class_ $ cssClass t] $ toHtml $ pack str
    toHtmlRaw (HeaderItem t str)
        | t == Title = h1_ $ toHtmlRaw $ pack str
        | otherwise = div_ [class_ $ cssClass t] $ toHtmlRaw $ pack str