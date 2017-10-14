{-# LANGUAGE RecordWildCards #-}
module Parsers.Question (parseQuest, parseTour) where

import           Constants.StringWorks  (parsingToken)
import           Data.Maybe             (fromMaybe)
import           Parsers.InlineSpace    (blankLines)
import           Parsers.Lines          (parseQFall)
import           Parsers.QN             (questModifier)
import           Structures.QNumber     (QModifier (Hard))
import           Structures.Quest       (Question (..), Tour (..))
import           Text.Megaparsec        (lookAhead, many, sepEndBy1, some,
                                         string, try)
import           Text.Megaparsec.String (Parser)



parseQuest :: Parser Question
parseQuest = do
    _ <- try $ lookAhead $ string $ parsingToken(undefined::Question)
    pre <- some parseQFall -- QText
    modifierM <- questModifier
    let modifier = fromMaybe (Hard 0) modifierM -- choice of default value is not optimal --TODO
    post <- many parseQFall -- in the current state question number modifier must after the full question text --TODO
    let fields = pre ++ post
    return Question{..}


parseTour :: Parser Tour
parseTour = do
    _ <- string $ parsingToken (undefined::Tour) -- a hack, I know
    quests <- parseQuest `sepEndBy1` blankLines
    let comment = Nothing -- todo fixme
    return Tour{..}
