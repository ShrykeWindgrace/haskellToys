module ImageLinkSpec
(imSpec)

where

import           Data.Maybe
import           ImageLinks
import           Test.Hspec
import           Text.Parsec

parseHelper :: String -> Either ParseError ILink
parseHelper = parse imageLink' ""

linkText :: String
linkText = "test.jpg"

showMaybe :: Maybe Integer -> String
showMaybe Nothing  = ""
showMaybe (Just n) = show n


showWidth :: ILink -> String
showWidth iLink
    | isNothing (width iLink) = ""
    | otherwise = "w = " ++ showMaybe (width iLink) ++ "px"

showHeight :: ILink -> String
showHeight iLink
    | isNothing (height iLink) = ""
    | otherwise = "w = " ++ showMaybe (height iLink) ++ "px"



linkToParseable :: ILink -> String
linkToParseable iLink = "(img " ++ showWidth iLink ++ showHeight iLink ++ link iLink ++ ")"

dataSet :: [ILink]
dataSet = [ILink linkText w h | w <- [Nothing, Just 600], h <- [Nothing, Just 400]]

dataSetString ::[String]
dataSetString = linkToParseable <$> dataSet

shoulders :: [Expectation]
shoulders = zipWith shouldBe (parseHelper <$> dataSetString) (Right <$> dataSet)

specs = (\x ->  describe x  (it "should be ok")) <$> dataSetString 

imSpec2 ::[Spec]
imSpec2 = zipWith ($) specs shoulders

imSpec :: Spec
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


