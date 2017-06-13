module Inline where


import Text.Parsec
import Text.Parsec.String
-- import Control.Monad (void)

emphText :: Parser String
emphText = do
  s <- between (char '_') (char '_') $ many1 $ noneOf "_\n"
  return $ "EMPH:" ++ s

emphText' :: Parser String
emphText' = ("<e>" ++) . (++ "</e>") <$> between (char '_') (char '_') (many1 $ noneOf "_\n")

emphText'' :: Parser Char
emphText'' = const 'E' <$> between (char '_') (char '_') (many1 $ noneOf "_\n")


stressedWord :: Parser String
stressedWord = do
    pref <- many letter
    _ <- char '`'
    s <- letter
    post <- many letter
    return $ pref ++ "<str>" ++ [s] ++ "</str>" ++ post