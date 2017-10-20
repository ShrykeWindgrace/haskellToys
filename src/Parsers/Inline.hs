module Parsers.Inline (stressedWord, regularWord, oneWord) where


import           Parsers.ImageLinks   (imageLink)
import           Parsers.InlineSpace  (wordEnd)
import           Parsers.Tech         (Parser)
import           Structures.Words     (OneWord (..))
import           Text.Megaparsec      (try, many, (<|>))
import           Text.Megaparsec.Char (char, letterChar, noneOf)


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
