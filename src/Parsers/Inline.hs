module Parsers.Inline where


import           Data.List          (intercalate)
import           Parsers.Tech       (lexeme)
import qualified Structures.Lines   as SL
import           Structures.Words
import           Text.Parsec
import           Text.Parsec.String


stressedWord :: Parser OneWord
stressedWord = do
    pref <- many letter
    _ <- char '`'
    s <- letter
    post <- many letter
    try $ lookAhead $ choice [() <$ try (oneOf " \n\t\r"), eof] -- end of word; TODO to refactor this parser
    return $ StressedWord pref s post


regularWord :: Parser OneWord
regularWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- noneOf "_`" -- does not start with emphasis token or a stress mark
    middle <- many $ noneOf " \n\t`" -- word ends with a space and does not contain stresses
    try $ lookAhead $ choice [() <$ try (oneOf " \n\t\r"), eof] -- end of word; TODO to refactor this parser

    return $ RegWord $ first:middle -- give back everything else


oneWord :: Parser OneWord
oneWord = lexeme $ try stressedWord <|> try  regularWord


{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> lexeme (many1 digit) <?> "decimal digit"


rawLine :: Parser String
rawLine = lexeme $ many1 $ noneOf "\r\n"

regLine :: Parser SL.Line
regLine = SL.Line <$> many1 oneWord -- or just many?
