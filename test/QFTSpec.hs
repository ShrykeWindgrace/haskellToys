{-# LANGUAGE TemplateHaskell #-}
module QFTSpec (spec) where

import           Constants.StringWorks
import           Data.DeriveTH
import           Helpers               (parseGen)
import           Parsers.Field
import           Structures.Quest
import           Test.Hspec
import           Test.QuickCheck
import           Text.Parsec

-- I know that this is an "orphan instance" [-Worphans]. But I don't need this instance anywhere else. Yet
$( derive makeArbitrary ''QFieldType ) -- Arbitrary instance for QFieldType to facilitate automated checks


parserHelper :: QFieldType -> Either ParseError QFieldType
parserHelper ft = parseGen (fieldType (parsingToken ft)) (parsingToken ft)


tester :: QFieldType -> Bool
tester x = Right x == parserHelper x

spec :: Spec
spec = describe "question field type parser" $ it "should correctly parse its own strings" $
    property $ \x -> tester (x :: QFieldType)
