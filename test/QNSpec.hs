module QNSpec (spec) where

import           Data.Either        (isLeft)
import           Helpers            (parseGen)
import           Parsers.QN
import           Structures.QNumber
import           Test.Hspec
import           Test.QuickCheck
import           Text.Megaparsec        

parserHelper :: Integer -> Either (ParseError (Token String) Dec) QModifier
parserHelper n = parseGen qHardReset' ("№№ " ++ show n)

tester :: Integer -> Bool
tester x
    | x < 0 = isLeft $ parserHelper x
    | otherwise = Right (Hard x) == parserHelper x

spec = describe "qHardReset'" $ it "should correctly parse hard reset" $
    property $ \x -> tester (x:: Integer)
