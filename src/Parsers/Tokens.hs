module Parsers.Tokens where

import           Parsers.InlineSpace
import           Render.StringWorks
import           Text.Parsec
import           Text.Parsec.String

inlineText = undefined
rawText = undefined
listLines = undefined

{-|
  List of recognised tokens as well as an endOfLine symbol

  regular lines can not begin with these symbols
-}
tokenList :: String
tokenList = "@?!-\n№#/=^<>["


-- |
--   parser for single-character tokens
tokenLines :: Char -> Parser String
tokenLines c =
  do _ <- char c
     spaces'
     _ <- optionMaybe $ char '\n'
     ss <- many1 (try inlineText <|> listLines)
     return $ "(" ++ tokenToString c ++ ")" ++ concat ss

-- |
--   parser for multicharacter tokens
longTokenLines :: String -> Parser String
longTokenLines str =
  do _ <- string str
     ss <- many1 $ try rawText
     return $ '(' : longTokenToString str ++ ")" ++ concat ss
