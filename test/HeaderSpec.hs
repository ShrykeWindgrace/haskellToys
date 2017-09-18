module HeaderSpec (spec) where

import           Data.Either        (isLeft)
import           Helpers            (parseGen)
import           Parsers.Header
import           Render.StringWorks
import           Structures.Header
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec
import           Text.Parsec.String
import Parsers.Debug  (seeNext)
parserHelper :: String -> Either ParseError Editor
parserHelper n = parseGen (seeNext 20 >> parseEditor) (edLine ++ " " ++ n ++ "\n")

tester :: String -> Bool
tester x =  Right (Editor x) == parserHelper x && valid x where
    valid s = not (0 == length x || '\n' `elem` s || last s `elem` "\t ") 

spec = do
    describe "parseEditor" $ it "should correctly parse editor" $
        property $ \x -> tester (x:: String)
    describe "manual empty string" $ it "empty string" $
        isLeft (parserHelper "") `shouldBe` True
