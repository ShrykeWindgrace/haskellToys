module Parsers.Inline where


import           Parsers.Tech       (lexeme, wordEnd)
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
    wordEnd
    return $ StressedWord pref s post


regularWord :: Parser OneWord
regularWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- noneOf "_`\n" -- does not start with emphasis token or a stress mark
    middle <- many $ noneOf " \n\t`" -- word ends with a space and does not contain stresses
    wordEnd
    return $ RegWord $ first:middle -- give back everything else


oneWord :: Parser OneWord
oneWord = try stressedWord <|> try  regularWord


{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> lexeme (many1 digit) <?> "decimal digit"


rawLine :: Parser String
rawLine = lexeme $ many1 $ noneOf "\r\n"

regLine :: Parser SL.Line
regLine = SL.Line <$> many1 oneWord -- or just many?
