{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Parsers.Question (parseQuest, parseTour, parseTournament) where


import           Constants.StringWorks (parsingToken)
import           Parsers.Header (headerParsers)
import           Parsers.InlineSpace   (blankLine, blankLines)
import           Parsers.Lines         (parseQFall)
import           Parsers.QN            (questModifier)
import           Parsers.Tech          (Parser)
import           Structures.Quest      (Question (..), Tour (..),
                                        Tournament (..))
import           Text.Megaparsec       (choice, dbg, lookAhead, many, many,
                                        sepEndBy1, some, try)
import           Text.Megaparsec.Char  (string)



parseQuest :: Parser Question
parseQuest = do
    _ <- try $ lookAhead $ string $ parsingToken(undefined::Question)
    pre <- some parseQFall -- QText
    modifier <- questModifier
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
