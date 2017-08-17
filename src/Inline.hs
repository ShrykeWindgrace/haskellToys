module Inline where


import           Data.List          (intercalate)
import           InlineSpace        (spaces')
import           Text.Parsec
import           Text.Parsec.String

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
    -- _ <- many $ char ' '  -- eat all spaces
    pref <- many letter
    _ <- char '`'
    s <- letter
    post <- many letter
    return $ pref ++ "<str>" ++ [s] ++ "</str>" ++ post


regularWord :: Parser String
regularWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- satisfy (/= '_') -- does not start with emphasis token
    middle <- many $ noneOf " _\n" -- word ends with a space
    return $ first:middle -- give back everything else
                          --
oneWord :: Parser String
oneWord = do
    spaces'  -- eat all spaces
    try stressedWord <|> try regularWord

emphText''' :: Parser String
emphText''' = ("<e>" ++) . (++ "</e>") <$> (intercalate " ") <$> between (char '_') (char '_') (many oneWord)
