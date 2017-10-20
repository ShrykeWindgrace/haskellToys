module Parsers.Debug (seeNext) where

import           Debug.Trace
import           Text.Megaparsec
import           Parsers.Tech

seeNext :: Int -> Parser ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  traceShowM out
  return ()
