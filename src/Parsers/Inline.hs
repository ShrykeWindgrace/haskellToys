module Parsers.Inline where


import           Data.List           (intercalate)
import           Parsers.InlineSpace (spaces')
import           Parsers.Tech        (lexeme)
import qualified Structures.Lines    as SL
import           Structures.Words
import           Text.Megaparsec        
import           Text.Megaparsec.String


stressedWord :: Parser OneWord
stressedWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    pref <- many letterChar
    _ <- char '`'
    s <- letterChar
    post <- many letterChar
    choice [() <$ try (oneOf " \n\t\r"), eof]
    return $ StressedWord pref s post


regularWord :: Parser OneWord
regularWord = do
    -- _ <- many $ char ' '  -- eat all spaces
    first <- satisfy (/= '_') -- does not start with emphasis token
    middle <- many $ noneOf " \n\t`" -- word ends with a space and does not contain stresses
    choice [() <$ try (oneOf " \n\t\r"), eof]

    return $ RegWord $ first:middle -- give back everything else


oneWord :: Parser OneWord
oneWord = do
    spaces'  -- eat all spaces
    try stressedWord <|> try regularWord


{-|
    Parse non-negative integer; eats all preceding space
-}
decimal :: Parser Integer
decimal = read <$> lexeme (some digitChar) <?> "decimal digit"


rawLine :: Parser String
rawLine = lexeme $ some $ noneOf "\r\n"

regLine :: Parser SL.Line
regLine = SL.Line <$> some oneWord -- or just many?
