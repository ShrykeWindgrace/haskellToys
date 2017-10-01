module LineSpec (spec) where

import           Helpers
import           Parsers.Lines
import           Structures.Composers
import           Structures.Lines
import           Structures.Words
import           Test.Hspec
import           Text.Megaparsec

parseHelper :: String -> Either (ParseError (Token String) Dec) Line
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
            parseGen pLines "-one line \n-two line  " `shouldBe` Right expectGood''

expectGood :: Line
expectGood = RegWord "строка" $:$ RegWord "с" $:$ Line [StressedWord "удар" 'е' "нием"]


expectGood' :: Line
expectGood' = Line $ RegWord <$> words "строка без ударения  "

expectGood'' :: ListLines
expectGood'' = ListLines [RegWord "one" $+$ RegWord "line", RegWord "two" $+$ RegWord "line"]
