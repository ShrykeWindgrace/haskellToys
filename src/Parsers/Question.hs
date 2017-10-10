{-# LANGUAGE RecordWildCards #-}
module Parsers.Question (parseQuest, parseTour) where

import           Data.Maybe             (fromJust)
import           Parsers.InlineSpace
import           Parsers.Lines          (parseQFall)
import           Parsers.QN             (questModifier)
import           Structures.Quest       (Question (..), Tour (..))
import           Text.Megaparsec        (many, sepEndBy1, some)
import           Text.Megaparsec.String (Parser)



parseQuest :: Parser Question
parseQuest = do
    pre <- many parseQFall -- QText
    modifierM <- questModifier
    let modifier = fromJust modifierM
    post <- some parseQFall -- answer must be there
    let fields = pre ++ post
    return Question{..}

parseTour :: Parser Tour
parseTour = do
    quests <- parseQuest `sepEndBy1` blankLines
    let comment = Nothing -- todo fixme
    return Tour{..}
