module InlineStructSpec (spec) where

import           Control.Monad    (zipWithM_)
import           Data.Either      (isLeft)
import           Helpers          (parseGen)
import           Parsers.Inline
import           Structures.Words
import           Test.Hspec
import           Text.Megaparsec             


parseHelper :: String -> Either (ParseError (Token String) Dec) OneWord
parseHelper = parseGen oneWord

dataSetStressGood :: [String]
dataSetStressGood = ["удар`ение", "`е"]

dataSetUnstressGood :: [String]
dataSetUnstressGood = ["слово", "а", "КАПСЛОК", "зАбОрЧиК", "цифры11", "знакПрепинания;", "без_пробела"]

dssgr :: [OneWord]
dssgr = [StressedWord "удар" 'е' "ние", StressedWord "" 'е' ""] ++ (RegWord <$> dataSetUnstressGood)

dataSetStressBad :: [String]
dataSetStressBad = ["", "сомелье`", "удар``ение", "уд`ар`ение"]

dataSetStressBadDescr :: [String]
dataSetStressBadDescr = ["пустая строка", "повисшее ударение", "два ударения подряд", "два ударения не подряд"]

shoulds :: [Expectation]
shoulds = zipWith shouldBe (parseHelper <$> (dataSetStressGood ++ dataSetUnstressGood)) (Right <$> dssgr)

shouldsNeg :: [Expectation]
shouldsNeg = (`shouldSatisfy` isLeft) <$> (parseHelper <$> dataSetStressBad)

its' = it <$> map (++ " should be ok") (dataSetStressGood ++ dataSetUnstressGood)

itsNeg' = it <$> map (++ " should not be ok") dataSetStressBadDescr

its :: [SpecWith ()]
its = zipWith ($)  (its' ++ itsNeg') (shoulds ++ shouldsNeg)

specs :: [SpecWith a -> SpecWith a]
specs = describe <$> (dataSetStressGood ++ dataSetUnstressGood ++ dataSetStressBad)

spec :: Spec
spec = zipWithM_ ($) specs its
