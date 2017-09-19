module HeaderSpec (spec) where

import           Data.Either        (isLeft)
import           Helpers            (parseGen)
import           Parsers.Header
import           Render.StringWorks (edLine)
import           Structures.Header
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec
import           Text.Parsec.String


parserHelper :: String -> Either ParseError Editor
parserHelper n = parseGen parseEditor (edLine ++ " " ++ n ++ "\n")

tester :: String -> Bool
tester x
    | null x = isLeft (parserHelper x) -- it parses ParseError
    | '\n' `elem` x = True -- short-circuit this case
    | '\r' `elem` x = True -- short-circuit this case
    | ' ' `elem` x = True -- short-circuit this case
    | '\t' `elem` x = True -- short-circuit this case
    | otherwise = Right (Editor x) == parserHelper x

spec = -- do
    describe "parseEditor" $ it "should correctly parse editor" $
        property $ tester
    -- describe "manual empty string" $ it "empty string" $
    --     isLeft (parserHelper "") `shouldBe` True
