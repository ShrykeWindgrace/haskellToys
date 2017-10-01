module Render.Html.Tech (htmlListFold, htmlListFoldRaw)


where


import           Lucid (HtmlT, ToHtml, toHtml, toHtmlRaw)


htmlListFold :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFold = htmlListFoldBase toHtml


htmlListFoldRaw :: (ToHtml a, Monad m) => [a] -> HtmlT m ()
htmlListFoldRaw = htmlListFoldBase toHtmlRaw


htmlListFoldBase :: (ToHtml a, Monad m) => (a -> HtmlT m ()) -> [a] -> HtmlT m ()
htmlListFoldBase fn = foldr1 mappend . map fn
