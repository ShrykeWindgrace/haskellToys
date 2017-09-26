{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Render.Html.Rend where 


import Structures.QNumber
import Structures.Words
import Data.Text hiding (foldr1, map)
import Lucid
import Data.Maybe (maybeToList)
import Structures.Quest
import Structures.Lines
import Data.Semigroup ((<>))

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


instance ToHtml Line where
    toHtml (Line list) = div_ $ do
        foldr1 (<>) (toHtml <$> list)

    toHtmlRaw (Line list) = div_ $ do
        foldr1 (<>) (toHtmlRaw <$> list)


-- instance ToHtml Question where
--     toHtml Question {..} = div_ [class_ "question"] $ do
--         toHtml modifier
--         div_ [class_ "questText"] $ do
--             htmlListFold text
--         div_ [class_ "answerText"] $ do
--             htmlListFold answer
--     toHtmlRaw Question {..} = div_ $ do
--         undefined


-- htmlListFold :: (ToHtml a) => [a] -> Html ()
-- htmlListFold = htmlListFoldBase toHtml

-- htmlListFoldRaw :: (ToHtml a) => [a] -> Html ()
-- htmlListFoldRaw = htmlListFoldBase toHtmlRaw

-- htmlListFoldBase :: (ToHtml a) => (a -> Html ()) -> [a] -> Html ()
-- htmlListFoldBase fn = (foldr1 (<>)) . (map fn)
