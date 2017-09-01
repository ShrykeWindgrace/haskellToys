module Structures.Quest where

import Structures.QNumber
import Structures.Lines


data Question = Question {
    modifier :: QModifierM,
    text :: [Line],
    answer :: [Line]
} deriving (Show)
