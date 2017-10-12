module LineSpec (spec) where

import           Helpers              (parseGen, ParseResult)
import           Parsers.Lines        (pLineInner, pLines, pLineExternal)
import           Structures.Composers (($+$), ($:$))
import           Structures.Lines     (Line (..), ListLines (..))
import           Structures.Words     (OneWord (..))
import           Test.Hspec           (Spec, describe, it, shouldBe)


parseHelper :: String -> ParseResult Line
parseHelper = parseGen pLineInner

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
    describe "\"-раз строчка -два строчка  \"" $ -- todo test with listlinesstr
        it ("should be well-parsed to " ++ show expectGood'') $
            parseGen pLineExternal "-one line \n-two line  " `shouldBe` Right (ListLinesStr expectGood'')

expectGood :: Line
expectGood = RegWord "строка" $:$ RegWord "с" $:$ Line [StressedWord "удар" 'е' "нием"]


expectGood' :: Line
expectGood' = Line $ RegWord <$> words "строка без ударения  "

expectGood'' :: ListLines
expectGood'' = ListLines [RegWord "one" $+$ RegWord "line", RegWord "two" $+$ RegWord "line"]
