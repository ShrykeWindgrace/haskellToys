module DecimalSpec where

import Helpers
import Inline
import Test.QuickCheck
import Test.Hspec

parserHelper = parseGen decimal



spec = describe "decimal parser" $ do
    it "should correctly parse positive integers" $
        property $
            \x -> parserHelper (show (abs x)) == Right ((abs x) :: Integer)