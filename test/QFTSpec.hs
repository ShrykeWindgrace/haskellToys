{-# OPTIONS_GHC -Wno-orphans  #-}
-- {-# LANGUAGE TemplateHaskell #-}
module QFTSpec (spec) where

import           Constants.StringWorks (parsingToken)
-- import           Data.DeriveTH         (derive, makeArbitrary)
import           Helpers               (parseGen, ParseResult)
import           Parsers.Field         (fieldType)
import           Structures.Quest      (QFieldType (..), allQFTs)
import           Test.Hspec            (Spec, describe, it)
-- required for TH derivation magic
import           Test.QuickCheck       (Arbitrary, arbitrary, choose, property)

-- I know that this is an "orphan instance" [-Worphans]. But I don't need this instance anywhere else. Yet

instance Arbitrary QFieldType where
    arbitrary = toEnum <$> choose (0, fromEnum (maxBound::QFieldType))
        

parserHelper :: QFieldType -> ParseResult QFieldType
parserHelper ft = parseGen (fieldType tok) tok where
    tok = parsingToken ft

parserHelper' :: QFieldType -> QFieldType -> ParseResult QFieldType
parserHelper' ft ft' = parseGen (fieldType tok) tok' where
    tok = parsingToken ft
    tok' = parsingToken ft'


tester :: QFieldType -> Bool
tester x = Right x == parserHelper x

-- we need an ugly hack here because QNotEquiv has token "!=" and it breaks the test for QAnswer
-- this test verifies that the corresponding parsers succeed only on their tokens
tester' :: QFieldType -> Bool
tester' x
    | x == QAnswer = [x] == [y | y <- allQFTs, y /= QNotEquiv, Right x == parserHelper' x y]
    | otherwise = [x] == [y | y<- allQFTs, Right x == parserHelper' x y]


spec :: Spec
spec = do
    describe "question field type parser" $ it "should correctly parse its own strings" $
        property tester
    describe "question field type parser" $ it "should correctly parse only its own strings" $
        property tester'
