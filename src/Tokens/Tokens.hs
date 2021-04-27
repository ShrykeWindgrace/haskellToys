{-# language StrictData #-}

module Tokens.Tokens where


data Token =
    QuestionStart | QuestionEnd |
    RemarkStart | RemarkEnd | -- []
    List |
    Answer |
    Equiv |
    NotEquiv |
    Comment |
    Source |
    Author |
    Underscore |
    ImgStart | ImgEnd |
    Stress Char |
    Regular Char |
    LJ |
    FullName |
    Date |
    Editor |
    Tour |
    SoftNumber |
    HardNumber
        deriving (Eq, Show)


parsingRepresentation :: Token -> String
parsingRepresentation QuestionStart = "?"
parsingRepresentation QuestionEnd   = error "double newline"
parsingRepresentation RemarkStart   = "["
parsingRepresentation RemarkEnd     = "]"
parsingRepresentation List          = "-"
parsingRepresentation Answer        = "!"
parsingRepresentation Equiv         = "="
parsingRepresentation NotEquiv      = "!="
parsingRepresentation Comment       = "/"
parsingRepresentation Source        = "^"
parsingRepresentation Author        = "@"
parsingRepresentation Underscore    = "_"
parsingRepresentation ImgStart      = "(img"
parsingRepresentation ImgEnd        = ")"
parsingRepresentation (Stress c)    = ['`', c]
parsingRepresentation (Regular c)   = [c]
parsingRepresentation LJ            = "###LJ"
parsingRepresentation FullName      = "###"
parsingRepresentation Date          = "#DATE"
parsingRepresentation Editor        = "#EDITOR"
parsingRepresentation Tour          = "##"
parsingRepresentation SoftNumber    = "№"
parsingRepresentation HardNumber    = "№№"
