module Structures.Words (OneWord (..), ILink(..)) where


data OneWord = StressedWord String Char String | RegWord String | ILinkStr ILink
    deriving (Eq)


instance Show OneWord where
    show (RegWord word) = word
    show (StressedWord before char after) = before ++ [char, '\x0301'] ++ after
    show (ILinkStr iLink) = show iLink
    -- show _ = undefined
    

data ILink = ILink {
    link   :: String,
    width  :: Maybe Integer,
    height :: Maybe Integer
    } deriving (Eq, Show)
