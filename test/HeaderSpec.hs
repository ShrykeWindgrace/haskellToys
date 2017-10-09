module HeaderSpec (spec) where

import           Constants.StringWorks (parsingToken)
import           Data.Either           (isLeft, isRight)
import           Helpers               (parseGen)
import           Parsers.Header        (parseEditor)
import           Structures.Header     (HeaderItem, HeaderItemType (Editor))
import           Test.Hspec            (Spec, describe, it)
import           Test.QuickCheck       (property)
import           Text.Megaparsec       (Dec, ParseError, Token)


parserHelper :: String -> Either (ParseError (Token String) Dec) HeaderItem
parserHelper n = parseGen parseEditor $ unwords [parsingToken Editor, n]

tester :: String -> Bool
tester x
    | null x = isLeft (parserHelper x) -- it parses ParseError
    | '\n' `elem` x = True -- short-circuit this case
    | '\r' `elem` x = True -- short-circuit this case
    | ' ' `elem` x = True -- short-circuit this case
    | '\t' `elem` x = True -- short-circuit this case
    | '`' `elem` x = True -- short-circuit this case
    | '_' `elem` x = True -- short-circuit this case
    | otherwise = isRight(parserHelper x)

spec :: Spec
spec = -- do
    describe "parseEditor" $ it "should correctly parse editor" $ property tester
    -- describe "manual empty string" $ it "empty string" $
    --     isLeft (parserHelper "") `shouldBe` True
