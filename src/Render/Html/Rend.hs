{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans  #-}

module Render.Html.Rend ()


where


import           Constants.StringWorks (cssClass, showNatural)
import           Control.Monad         (when, unless)
import           Data.List             (sort)
import           Data.Maybe            (fromJust, isJust, maybeToList)
import           Data.Text             hiding (foldr1, map)
import           Lucid                 (ToHtml, toHtml, toHtmlRaw, HtmlT, div_, class_, span_, img_, src_, width_, ol_, li_, hr_, h1_, h2_)
import           Render.Html.Tech      (htmlListFold, htmlListFoldRaw, htmlListFoldBr)
import           Structures.Header (HeaderItem(..), HeaderItemType(Title))
import           Structures.Lines (Line(Line, ListLinesStr), ListLines(ListLines))
import           Structures.QNumber (QModifier(Soft))
import           Structures.Quest (Question(..), QField(..), QFieldType(QText), Tour(..), Tournament(..), Comment(..))
import           Structures.Words (OneWord(RegWord, StressedWord, ILinkStr), ILink(..))

instance ToHtml QModifier where

    toHtml a = div_ [class_ $ cssClass a] $ do
            span_ [class_ "questWord"] $ toHtml $ showNatural a
            toHtml (" " :: String)
            span_ [class_ "questNumber"] $ toHtml (show a ++ ":")-- line is escaped

    toHtmlRaw a = div_ [class_ $ cssClass a] $ do
            span_ [class_ "questWord"] $ toHtmlRaw $ showNatural a
            span_ [class_ "questNumber"] $ toHtmlRaw (show a ++ ":") -- line is escaped


instance ToHtml OneWord where
    toHtml (RegWord str) = toHtml str
    toHtml (StressedWord pre c post) = do
        toHtml (pre ++ [c])
        toHtmlRaw ("&#x301;" :: String) -- need Raw, otherwise '&' is escaped
        toHtml post
    toHtml (ILinkStr iLink) = img_ $ [src_ $ pack $ link iLink] ++
        ( width_ . pack . show <$> maybeToList (width iLink) ) ++
        ( width_ . pack . show <$> maybeToList (height iLink) )


    toHtmlRaw (RegWord str) = toHtmlRaw str
    toHtmlRaw a@StressedWord{} = toHtmlRaw (show a)
    toHtmlRaw  (ILinkStr iLink) = img_ $ [src_ $ pack $ link iLink] ++
        ( width_ . pack . show <$> maybeToList (width iLink) ) ++
        ( width_ . pack . show <$> maybeToList (height iLink) )


instance ToHtml Line where
    toHtml (Line list)        = htmlListFold list
    toHtml (ListLinesStr lls) = toHtml lls

    toHtmlRaw (Line list)        = htmlListFoldRaw list
    toHtmlRaw (ListLinesStr lls) = toHtmlRaw lls


instance ToHtml ListLines where
    toHtml (ListLines list) = ol_ $
        foldr1 mappend $ map (li_ [] . toHtml) list

    toHtmlRaw (ListLines list) = ol_ $
        foldr1 mappend $ map (li_ [] . toHtmlRaw) list

instance ToHtml Question where
    toHtml Question {..} = div_ [class_ $ cssClass (undefined :: Question)] $ do
        maybe (toHtml $ Soft "без номера") toHtml modifier
        htmlListFold $ sort fields
        hr_[]

    toHtmlRaw  Question {..} = div_ [class_ $ cssClass (undefined :: Question)] $ do
        maybe (toHtml $ Soft "без номера") toHtmlRaw modifier
        htmlListFoldRaw $ sort fields
        hr_[]


instance ToHtml QField where
    toHtml (QField t list) = div_ [class_ $ cssClass t] $ do
        unless (t==QText) (span_ [class_ $ cssClass t] $ toHtml (showNatural t ++ ": ")) -- should we put spaces as a part of css?
        htmlListFoldBr toHtml list

    toHtmlRaw (QField t list) = div_ [class_ $ cssClass t] $ do
        unless (t==QText) (span_ [class_ $ cssClass t] $ toHtml (showNatural t ++ ": ")) -- should we put spaces as a part of css?
        htmlListFoldBr toHtmlRaw list



instance ToHtml HeaderItem where
    toHtml = toHtmlHeader toHtml
    toHtmlRaw = toHtmlHeader toHtmlRaw


{-|
    Helper function to avoid code duplication for raw and non-raw html functions
-}
toHtmlHeader :: (Monad m) => (Line -> HtmlT m ()) -> HeaderItem ->  HtmlT m ()
toHtmlHeader fn (HeaderItem t str)
    | t == Title = h1_ $ fn str
    | otherwise = div_ [class_ $ cssClass t]  (fn str)


instance ToHtml Tour where
    toHtml t@Tour{..} = div_ [class_ $ cssClass t] $ do
        h2_ $ toHtml $ "Тур " ++ maybe "без номера" show tModifier
        when (isJust comment) $
            div_ [class_ $ cssClass (undefined::Comment) ] $ toHtml $ unComment $ fromJust comment
        htmlListFold quests

    toHtmlRaw t@Tour{..} = div_ [class_ $ cssClass t] $ do
        when (isJust comment) $
            div_ [class_ $ cssClass (undefined::Comment)  ] $ toHtmlRaw $ unComment $ fromJust comment
        htmlListFoldRaw quests

instance ToHtml Tournament where
    toHtml t@Tournament{..} = div_ [class_ $ cssClass t] $ do
        htmlListFold header
        htmlListFold tours
        -- todo comments
        -- todo numbering in questions and tours
    toHtmlRaw = toHtml
