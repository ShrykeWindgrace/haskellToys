module Parsers.Inline where


import           Data.List           (intercalate)
import           Parsers.InlineSpace (spaces')
import           Parsers.Tech        (lexeme)
import           Structures.Words
import           Text.Parsec
import           Text.Parsec.String
import qualified Structures.Lines as SL


stressedWord' :: Parser OneWord
stressedWord' = do
    -- _ <- many $ char ' '  -- eat all spaces
    pref <- many letter
    _ <- char '`'
    s <- letter
    post <- many letter --TODO words with two non-consecutive stresses are well-parsed, even if they should not
    choice [() <$ try (oneOf " \n\t\r"), eof]
    return $ StressedWord pref s post


regularWord' :: Parser OneWord
regularWord' = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- satisfy (/= '_') -- does not start with emphasis token
    middle <- many $ noneOf " \n\t`" -- word ends with a space and does not contain stresses
    choice [() <$ try (oneOf " \n\t\r"), eof]

    return $ RegWord $ first:middle -- give back everything else


oneWord' :: Parser OneWord
oneWord' = do
    spaces'  -- eat all spaces
    try stressedWord' <|> try regularWord'


{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> lexeme (many1 digit) <?> "decimal digit"


rawLine :: Parser String
rawLine = lexeme $ many $ noneOf "\r\n"

regLine :: Parser SL.Line
regLine = SL.Line <$> many1 oneWord' -- or just many?
