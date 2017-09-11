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
spec = describe "basic line" $
 it "should be well-parsed" $
  (parseHelper "b`asic line") `shouldBe` Right (Line [StressedWord "b" 'a' "sic", RegWord "line"])
