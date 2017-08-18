module InlineSpec (inlineSpec) where

import           Inline
import           Test.Hspec
import           Text.Parsec


isLeft :: Either a b -> Bool
isLeft (Right _) = False
isLeft (Left _)  = True

parseHelper :: String -> Either ParseError String
parseHelper = parse stressedWord ""

dataSetStressGood :: [String]
dataSetStressGood = ["удар`ение", "`е"]

dssgr :: [String]
dssgr = ["удар<str>е</str>ние", "<str>е</str>"]

dataSetStressBad :: [String]
dataSetStressBad = ["", "`", "сомелье`", "нет", "удар``ение", "уд`ар`ение"]

shoulds :: [Expectation]
shoulds = zipWith shouldBe (parseHelper <$> dataSetStressGood) (Right <$> dssgr)

shouldsNeg :: [Expectation]
shouldsNeg = (`shouldSatisfy` isLeft) <$> (parseHelper <$> dataSetStressBad)

its' = it <$> map (++ " should be ok") dataSetStressGood

itsNeg' = it <$> map (++ " should not be ok") dataSetStressBad

its :: [SpecWith ()]
its = zipWith ($)  (its' ++ itsNeg') (shoulds ++ shouldsNeg)

specs :: [SpecWith a -> SpecWith a]
specs = describe <$> (dataSetStressGood ++ dataSetStressBad)

imSpec2' ::[Spec]
imSpec2' = zipWith ($) specs its

inlineSpec :: Spec
inlineSpec = sequence_ imSpec2'
