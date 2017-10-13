{-# LANGUAGE RecordWildCards #-}
module Parsers.Question (parseQuest, parseTour) where

import           Data.Maybe             (fromMaybe)
import           Parsers.InlineSpace    (blankLines)
import           Parsers.Lines          (parseQFall)
import           Parsers.QN             (questModifier)
import           Structures.Quest       (Question (..), Tour (..))
import           Text.Megaparsec        (many, sepEndBy1, some)
import           Text.Megaparsec.String (Parser)
import  Structures.QNumber



parseQuest :: Parser Question
parseQuest = do
    pre <- some parseQFall -- QText
    modifierM <- questModifier
    let modifier = fromMaybe (Hard 0) modifierM -- choice of default value is not optimal --TODO
    post <- many parseQFall -- in the current state question number modifier must after the full question text --TODO
    let fields = pre ++ post
    return Question{..}


parseTour :: Parser Tour
parseTour = do
    quests <- parseQuest `sepEndBy1` blankLines
    let comment = Nothing -- todo fixme
    return Tour{..}
