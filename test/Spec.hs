import           ImageLinks
import           Text.Parsec
import           Test.Hspec
import ImageLinkSpec

-- isRight :: Either a b -> Bool
-- isRight (Left _) = False
-- isRight (Right _) = True


-- isLeft :: Either a b -> Bool
-- isLeft = not . isRight

-- parseHelper = parse imageLink' ""

-- goodDataset :: [String]
-- goodDataset = [
    
--     "(img w = 12px test.jpg)",
--     "(img w = 12px h=13px test.jpg)",
--     "(img h = 12px w=13px test.jpg)"
--     ]

-- badDataset = ["(pic "]

-- testResults = parseHelper <$> goodDataset
-- testResults2 = parseHelper <$> badDataset

main = hspec $ do
    imSpec