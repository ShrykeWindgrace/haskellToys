module Render.Html.Tech (htmlListFold, htmlListFoldRaw)


where


import           Lucid (HtmlT, ToHtml, toHtml, toHtmlRaw)
import           Data.List


htmlListFold :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFold = htmlListFoldBase toHtml


htmlListFoldRaw :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFoldRaw = htmlListFoldBase toHtmlRaw


htmlListFoldBase :: (ToHtml a, Monad m) => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBase fn lst = foldr1 mappend ( intersperse (toHtml " ") (map fn lst))
