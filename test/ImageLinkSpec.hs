module ImageLinkSpec
(imSpec, imSpec2)

where

import           Data.Maybe
import           ImageLinks
import           Test.Hspec
import           Text.Parsec

parseHelper :: String -> Either ParseError ILink
parseHelper = parse imageLinkP ""

linkText :: String
linkText = "test.jpg"


showMaybe :: Maybe Integer -> String
showMaybe Nothing  = ""
showMaybe (Just n) = show n


showWidth :: ILink -> String
showWidth iLink
    | isNothing (width iLink) = " "
    | otherwise = "w = " ++ showMaybe (width iLink) ++ "px "

showHeight :: ILink -> String
showHeight iLink
    | isNothing (height iLink) = " "
    | otherwise = "h = " ++ showMaybe (height iLink) ++ "px "



linkToParseable :: ILink -> String
linkToParseable iLink = "(img " ++ showWidth iLink ++ showHeight iLink ++ link iLink ++ ")"

dataSet :: [ILink]
dataSet = [ILink linkText w h | w <- [Nothing, Just 600], h <- [Nothing, Just 400]]

dataSetString ::[String]
dataSetString = linkToParseable <$> dataSet

shoulds :: [Expectation]
shoulds = zipWith shouldBe (parseHelper <$> dataSetString) (Right <$> dataSet)

its' = it <$> (map (++ " should be ok") dataSetString)

its :: [SpecWith ()]
its = zipWith ($)  its' shoulds

specs :: [SpecWith a -> SpecWith a]
specs = describe <$> dataSetString

imSpec2' ::[Spec]
imSpec2' = zipWith ($) specs its

imSpec2 :: Spec
imSpec2 = sequence_ imSpec2'

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