module HeaderSpec (spec) where

import           Data.Either        (isLeft)
import           Helpers            (parseGen)
import           Parsers.Header
import           Render.StringWorks
import           Structures.Header
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec

parserHelper :: String -> Either ParseError Editor
parserHelper n = parseGen parseEditor (edLine ++ " " ++ n ++"\n")

tester :: String -> Bool
tester x =  Right (Editor x) == parserHelper x || null ((filter (/= ' ') . filter (/= '\t')) x) 

spec = describe "parseEditor" $ it "should correctly parse editor" $
    property $ \x -> tester (x:: String)
