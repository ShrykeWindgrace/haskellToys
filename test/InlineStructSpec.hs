module InlineStructSpec (spec) where

import           Control.Monad    (zipWithM_)
import           Data.Either      (isLeft)
import           Helpers          (parseGen)
import           Parsers.Inline
import           Structures.Words
import           Test.Hspec
import           Text.Parsec      (ParseError)


parseHelper :: String -> Either ParseError OneWord
parseHelper = parseGen stressedWord'

dataSetStressGood :: [String]
dataSetStressGood = ["удар`ение", "`е"]

dssgr :: [OneWord]
dssgr = [StressedWord "удар" 'е' "ние", StressedWord "" 'е' ""]

dataSetStressBad :: [String]
dataSetStressBad = ["", "`", "сомелье`", "нет", "удар``ение", "уд`ар`ение"]

dataSetStressBadDescr :: [String]
dataSetStressBadDescr = ["пустая строка", "только ударение", "повисшее ударение", "нет ударения", "два ударения подряд", "два ударения не подряд"]

shoulds :: [Expectation]
shoulds = zipWith shouldBe (parseHelper <$> dataSetStressGood) (Right <$> dssgr)

shouldsNeg :: [Expectation]
shouldsNeg = (`shouldSatisfy` isLeft) <$> (parseHelper <$> dataSetStressBad)

its' = it <$> map (++ " should be ok") dataSetStressGood

itsNeg' = it <$> map (++ " should not be ok") dataSetStressBadDescr

its :: [SpecWith ()]
its = zipWith ($)  (its' ++ itsNeg') (shoulds ++ shouldsNeg)

specs :: [SpecWith a -> SpecWith a]
specs = describe <$> (dataSetStressGood ++ dataSetStressBad)

spec :: Spec
spec = zipWithM_ ($) specs its