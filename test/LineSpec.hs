module LineSpec (spec) where

import           Helpers
import           Parsers.Lines
import           Structures.Lines
import           Structures.Words
import           Test.Hspec
import           Text.Parsec        hiding (Line)
import           Text.Parsec.String

parseHelper = parseGen pLine

spec :: Spec
spec = do
    describe "b`asic line" $
        it ("should be well-parsed to " ++ show expectGood) $
            (parseHelper "b`asic line") `shouldBe` Right expectGood
    describe "basic line" $
        it ("should be well-parsed to " ++ show expectGood') $
            (parseHelper "basic line") `shouldBe` Right (expectGood')


expectGood :: Line
expectGood = Line [StressedWord "b" 'a' "sic", RegWord "line"]


expectGood' :: Line
expectGood' = Line [RegWord "basic", RegWord "line"]