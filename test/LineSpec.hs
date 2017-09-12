module LineSpec (spec) where

import           Helpers
import           Parsers.Lines
import           Structures.Lines
import           Structures.Words
import           Test.Hspec
import           Text.Parsec      (ParseError)

parseHelper :: String -> Either ParseError Line
parseHelper = parseGen pLine

spec :: Spec
spec = do
    describe "\"строка с удар`ением\"" $
        it ("should be well-parsed to " ++ show expectGood) $
            parseHelper "строка с удар`ением" `shouldBe` Right expectGood
    describe "\"строка без ударения  \"" $
        it ("should be well-parsed to " ++ show expectGood') $
            parseHelper "строка без ударения  " `shouldBe` Right expectGood'
    describe "\"-раз строчка -два строчка  \"" $
        it ("should be well-parsed to " ++ show expectGood'') $
            parseGen pLines "-раз строчка \n-два строчка  \n" `shouldBe` Right expectGood''           

expectGood :: Line
expectGood = Line [RegWord "строка", RegWord "с", StressedWord "удар" 'е' "нием"]


expectGood' :: Line
expectGood' = Line $ RegWord <$> words "строка без ударения  "

expectGood'' :: ListLines
expectGood'' = ListLines $ Line <$> [[RegWord "раз", RegWord "строчка"], [RegWord "два", RegWord "строчка"]]