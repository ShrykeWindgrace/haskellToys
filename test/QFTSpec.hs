{-# LANGUAGE TemplateHaskell #-}
module QFTSpec (spec) where

import           Data.DeriveTH
import           Data.Either               (isLeft)
import           Helpers                   (parseGen)
import           Parsers.Field
import           Structures.Quest
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Text.Megaparsec        


$( derive makeArbitrary ''QFieldType ) -- Arbitrary instance for QFieldType to facilitate automated checks


parserHelper :: QFieldType -> Either (ParseError (Token String) Dec) QFieldType
parserHelper ft = parseGen (fieldType (show ft)) (show ft)


tester :: QFieldType -> Bool
tester x = Right x == parserHelper x

spec = describe "question field type parser" $ it "should correctly parse its own strings" $
    property $ \x -> tester (x :: QFieldType)
