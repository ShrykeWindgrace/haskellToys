module PermutationTest (runTest) where

-- import           Control.Monad
import           Text.Parsec
import           Text.Parsec.Perm
import           Text.Parsec.String

weiP :: Parser Char
weiP = char 'w'

heiP :: Parser Char
heiP = char 'h'

toMaybe :: Parser a -> Parser (Maybe a)
toMaybe _parser = Just <$> try _parser


perms :: Parser [Maybe Char]
perms = permute ( (\x y -> [x,y]) <$?> (Nothing,  toMaybe weiP) <|?> (Nothing,  toMaybe heiP))

testParser = parse perms ""

runTest x = case testParser x of
    (Right y) -> "ok, " ++ show y
    (Left  y) -> "fail, " ++ show y
