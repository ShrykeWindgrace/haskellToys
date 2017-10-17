{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Parsers.Question (parseQuest, parseTour, parseTournament) where

import           Constants.StringWorks  (parsingToken)
import           Data.Maybe             (fromMaybe)
import           Parsers.InlineSpace    (blankLine, blankLines)
import           Parsers.Lines          (parseQFall)
import           Parsers.QN             (questModifier)
import           Parsers.Header
import           Structures.QNumber     (QModifier (Soft))
import           Structures.Quest       (Question (..), Tour (..), Tournament(..))
import           Text.Megaparsec        (lookAhead, many, sepEndBy1, some,
                                         string, try, choice, dbg, many)
import           Text.Megaparsec.String (Parser)



parseQuest :: Parser Question
parseQuest = do
    _ <- try $ lookAhead $ string $ parsingToken(undefined::Question)
    pre <- some parseQFall -- QText
    modifierM <- questModifier
    let modifier = fromMaybe (Soft "без номера") modifierM -- choice of default value is not optimal --TODO
    post <- many parseQFall -- in the current state question number modifier must after the full question text --TODO
    let fields = pre ++ post
    return Question{..}


parseTour :: Parser Tour
parseTour = do
    _ <- string $ parsingToken (undefined::Tour) -- a hack, I know
    _ <- many blankLine
    quests <- parseQuest `sepEndBy1` blankLines
    let comment = Nothing -- todo fixme
    let tModifier = Nothing -- todo implement parsing and modify 4s format 
    return Tour{..}


parseTournament :: Parser Tournament
parseTournament = do
    header <- some $ try $ many blankLine >> choice headerParsers
    _ <- many blankLine
    tours <- some $ dbg "tour" parseTour

    let commentTNT = Nothing -- todo implement
    return Tournament{..}