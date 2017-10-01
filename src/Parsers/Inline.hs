module Parsers.Inline where


import           Parsers.ImageLinks     (imageLink)
import           Parsers.Tech
import           Structures.Words       (OneWord (..))
import           Text.Megaparsec
-- import           Text.Megaparsec.Char
import           Text.Megaparsec.String (Parser)


stressedWord :: Parser OneWord
stressedWord = do
    pref <- many letterChar
    _ <- char '`'
    s <- letterChar
    post <- many letterChar
    wordEnd
    return $ StressedWord pref s post


regularWord :: Parser OneWord
regularWord = do
    first <- noneOf "_`\n\t" -- does not start with emphasis token or a stress mark
    middle <- many $ noneOf " \n\t`" -- word ends with a space and does not contain stresses
    wordEnd
    return $ RegWord $ first:middle -- give back everything else


oneWord :: Parser OneWord
oneWord = try (ILinkStr <$> imageLink) <|> try stressedWord <|> try  regularWord
