module Parsers.Debug (seeNext) where

import           Text.Parsec
import           Text.Parsec.String
import           Debug.Trace

seeNext :: Int -> Parser ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  traceShowM out
  return ()
