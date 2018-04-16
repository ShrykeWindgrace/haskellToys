module Parsers.Debug (seeNext) where

import           Debug.Trace (traceShowM)
import           Text.Megaparsec (getParserState, stateInput)
import           Parsers.Tech (Parser)

seeNext :: Int -> Parser ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  traceShowM out
  -- return ()
