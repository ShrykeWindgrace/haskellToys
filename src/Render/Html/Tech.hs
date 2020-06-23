module Render.Html.Tech (htmlListFold, htmlListFoldRaw, htmlListFoldBr)


where


import           Lucid (HtmlT, ToHtml, toHtml, toHtmlRaw, br_)
import           Data.List (intersperse)
import           Data.Foldable

htmlListFold :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFold = htmlListFoldBase toHtml


htmlListFoldRaw :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFoldRaw = htmlListFoldBase toHtmlRaw


htmlListFoldBase :: Monad m => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBase fn lst = fold $ intersperse (toHtml " ") (fn <$> lst)


htmlListFoldBr :: Monad m => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBr fn lst = fold $ intersperse (br_[]) (fn <$> lst)
