module ImageLinkSpec
(spec)

where

import           Control.Monad      (zipWithM_)
import           Data.Maybe         (isNothing)
import           Helpers            (parseGen, ParseResult)
import           Parsers.ImageLinks (imageLink)
import           Structures.Words   (ILink (..))
import           Test.Hspec         (Arg, Expectation, Spec, SpecWith, describe,
                                     it)
import           Test.Hspec.Megaparsec (shouldParse)                                     

parseHelper :: String -> ParseResult ILink
parseHelper = parseGen imageLink

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
shoulds = zipWith shouldParse (parseHelper <$> dataSetString) dataSet

its' :: [Expectation -> SpecWith (Arg Expectation)]
its' = it <$> map (++ " should be ok") dataSetString

its :: [SpecWith ()]
its = zipWith ($)  its' shoulds

specs :: [SpecWith a -> SpecWith a]
specs = describe <$> dataSetString

specAuto :: Spec
specAuto = zipWithM_ ($) specs its

specManual :: Spec
specManual = do
    describe "(img test.jpg)" $
        it "should be well parsed to \"test.jpg\"" $
            parseHelper "(img test.jpg)" `shouldParse` ILink "test.jpg" Nothing Nothing

    describe "(img h=400px test.jpg)" $
        it "should be well-parsed to link and height" $
            parseHelper "(img h=400px test.jpg)" `shouldParse` ILink "test.jpg" Nothing (Just 400)

    describe "(img w=600px test.jpg)" $
        it "should be well-parsed to link and width" $
            parseHelper "(img w=600px test.jpg)" `shouldParse` ILink "test.jpg" (Just 600) Nothing

    describe "(img w=600px h = 400px test.jpg)" $
        it "should be well-parsed to link and width" $
             parseHelper "(img w=600px h = 400px test.jpg)" `shouldParse` ILink "test.jpg" (Just 600) (Just 400)


    describe "(img h=400px w = 600px test.jpg)" $
        it "should be well-parsed to link and width" $
            parseHelper "(img h=400px w = 600px test.jpg)" `shouldParse` ILink "test.jpg" (Just 600) (Just 400)


spec :: Spec
spec = specManual >> specAuto
