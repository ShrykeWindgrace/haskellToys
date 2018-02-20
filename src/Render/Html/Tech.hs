module Render.Html.Tech (htmlListFold, htmlListFoldRaw, htmlListFoldBr)


where


import           Lucid (HtmlT, ToHtml, toHtml, toHtmlRaw, br_)
import           Data.List (intersperse)


htmlListFold :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFold = htmlListFoldBase toHtml


htmlListFoldRaw :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFoldRaw = htmlListFoldBase toHtmlRaw


htmlListFoldBase :: (ToHtml a, Monad m) => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBase fn lst = foldr1 mappend ( intersperse (toHtml " ") (map fn lst))


htmlListFoldBr :: (ToHtml a, Monad m) => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBr fn lst = foldr1 mappend (intersperse (br_[]) (map fn lst))
