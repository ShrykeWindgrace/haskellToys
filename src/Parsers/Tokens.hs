module Parsers.Tokens (tokenList) where

{-|
  List of recognised tokens

  regular lines can not begin with these symbols
-}
tokenList :: String
tokenList = "@?!-\n№#/=^<>["
