import           ImageLinks
import           Text.Parsec
import           Test.Hspec

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True


isLeft :: Either a b -> Bool
isLeft = not . isRight

parseHelper = parse imageLink' ""

goodDataset :: [String]
goodDataset = [
    
    "(img w = 12px test.jpg)",
    "(img w = 12px h=13px test.jpg)",
    "(img h = 12px w=13px test.jpg)"
    ]

badDataset = ["(pic "]

testResults = parseHelper <$> goodDataset
testResults2 = parseHelper <$> badDataset

main = hspec $ do
    describe "(img test.jpg)" $ do
        it "should be well parsed to \"test.jpg\"" $
            (parseHelper "(img test.jpg)") `shouldBe` (Right $ ILink "test.jpg" Nothing Nothing)
    
    describe "(img h=400px test.jpg)" $ do
        it "should be well-parsed to link and height" $
            (parseHelper "(img h=400px test.jpg)") `shouldBe` (Right $ ILink "test.jpg" Nothing (Just 400))
    
    describe "(img w=600px test.jpg)" $ do
        it "should be well-parsed to link and width" $
            (parseHelper "(img w=600px test.jpg)") `shouldBe` (Right $ ILink "test.jpg" (Just 600) Nothing)
    