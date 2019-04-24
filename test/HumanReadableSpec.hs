module HumanReadableSpec (spec) where

import           Constants.StringWorks (showNatural)
import           Structures.QNumber    (QModifier (..))
import           Structures.Quest      (QFieldType (..))
import           Test.Hspec            (Spec, describe, it, shouldBe)
spec :: Spec
spec = do
    describe "Rendering" $ do
        it "Вопрос" $ do
            showNatural (Soft "") `shouldBe` "Вопрос"
            showNatural (Hard 0) `shouldBe` "Вопрос"
        it "Ответ" $ do
            showNatural (QAnswer) `shouldBe` "Ответ"
            showNatural (QEquiv) `shouldBe` "Зачёт"
