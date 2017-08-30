module Parsers.Inline where


import           Data.List           (intercalate)
import           Parsers.InlineSpace (spaces')
import           Parsers.Tech        (lexeme)
import           Structures.Words
import           Text.Parsec
import           Text.Parsec.String


stressedWord :: Parser String
stressedWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    pref <- many letter
    _ <- char '`'
    s <- letter
    post <- many letter --TODO words with two non-consecutive stresses are well-parsed, even if they should not
    choice [() <$ try (oneOf " \n\t\r"), eof]
    return $ pref ++ "<str>" ++ [s] ++ "</str>" ++ post


stressedWord' :: Parser OneWord
stressedWord' = do
    -- _ <- many $ char ' '  -- eat all spaces
    pref <- many letter
    _ <- char '`'
    s <- letter
    post <- many letter --TODO words with two non-consecutive stresses are well-parsed, even if they should not
    choice [() <$ try (oneOf " \n\t\r"), eof]
    return $ StressedWord pref s post

regularWord :: Parser String
regularWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- satisfy (/= '_') -- does not start with emphasis token
    middle <- many $ noneOf " _\n\t`" -- word ends with a space and does not contain stresses
    return $ first:middle -- give back everything else
                          --

regularWord' :: Parser OneWord
regularWord' = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- satisfy (/= '_') -- does not start with emphasis token
    middle <- many $ noneOf " _\n\t`" -- word ends with a space and does not contain stresses
    return $ RegWord $ first:middle -- give back everything else


oneWord :: Parser String
oneWord = do
    spaces'  -- eat all spaces
    try stressedWord <|> try regularWord


oneWord' :: Parser OneWord
oneWord' = do
    spaces'  -- eat all spaces
    try stressedWord' <|> try regularWord'


{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> lexeme (many1 digit) <?> "decimal digit"
