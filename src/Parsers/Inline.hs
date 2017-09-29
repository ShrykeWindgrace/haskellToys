module Parsers.Inline where


import           Parsers.ImageLinks (imageLink)
import           Parsers.Tech       (wordEnd)
import           Structures.Words   (OneWord (..))
import           Text.Parsec        (char, letter, many, noneOf, try, (<|>))
import           Text.Parsec.String (Parser)


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
    first <- noneOf "_`\n\t" -- does not start with emphasis token or a stress mark
    middle <- many $ noneOf " \n\t`" -- word ends with a space and does not contain stresses
    wordEnd
    return $ RegWord $ first:middle -- give back everything else


oneWord :: Parser OneWord
oneWord = try (ILinkStr <$> imageLink) <|> try stressedWord <|> try  regularWord
