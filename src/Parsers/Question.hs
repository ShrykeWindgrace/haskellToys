{-# LANGUAGE RecordWildCards #-}
module Parsers.Question (parseQuest) where

import           Parsers.Lines          (parseQFall)
import           Parsers.QN             (questModifier)
import           Text.Megaparsec        (many, some)
import           Text.Megaparsec.String (Parser)
import           Structures.Quest       (Question(..))
import           Data.Maybe             (fromJust)



parseQuest :: Parser Question
parseQuest = do
    pre <- many parseQFall -- QText
    modifierM <- questModifier
    let modifier = fromJust modifierM
    post <- some parseQFall -- answer must be there
    let fields = pre ++ post
    return Question{..}
