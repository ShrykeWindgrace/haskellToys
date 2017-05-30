module Inline where


import Text.Parsec
import Text.Parsec.String
-- import Control.Monad (void)

emphText :: Parser String
emphText = do
  s <- between (char '_') (char '_') $ many1 $ noneOf "\n"
  return $ "EMPH:" ++ s