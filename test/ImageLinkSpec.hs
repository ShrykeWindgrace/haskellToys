module ImageLinkSpec
(imSpec)

where

import           ImageLinks
import           Test.Hspec
import           Text.Parsec

parseHelper = parse imageLink' ""


imSpec = do
    describe "(img test.jpg)" $
        it "should be well parsed to \"test.jpg\"" $
            parseHelper "(img test.jpg)" `shouldBe` (Right $ ILink "test.jpg" Nothing Nothing)

    describe "(img h=400px test.jpg)" $
        it "should be well-parsed to link and height" $
            parseHelper "(img h=400px test.jpg)" `shouldBe` (Right $ ILink "test.jpg" Nothing (Just 400))

    describe "(img w=600px test.jpg)" $
        it "should be well-parsed to link and width" $
            parseHelper "(img w=600px test.jpg)" `shouldBe` (Right $ ILink "test.jpg" (Just 600) Nothing)

    describe "(img w=600px h = 400px test.jpg)" $
        it "should be well-parsed to link and width" $
             parseHelper "(img w=600px h = 400px test.jpg)" `shouldBe` (Right $ ILink "test.jpg" (Just 600) (Just 400))


    describe "(img h=400px w = 600px test.jpg)" $
        it "should be well-parsed to link and width" $
            parseHelper "(img h=400px w = 600px test.jpg)" `shouldBe` (Right $ ILink "test.jpg" (Just 600) (Just 400))


