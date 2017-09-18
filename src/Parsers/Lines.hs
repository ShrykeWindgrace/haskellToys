module Parsers.Lines (pLine, pLines) where

import           Parsers.Inline
import           Parsers.InlineSpace (spaces')
import           Parsers.Tech        (lexeme)
import           Structures.Lines
import           Text.Parsec         hiding (Line)
import           Text.Parsec.String
import Parsers.Debug

pLine :: Parser Line
pLine = do
    words_ <- (seeNext 50 >> oneWord) `sepEndBy1` spaces'
    lookAhead $ eof <|> (() <$ newline)
    return $ Line words_
    
    

pLines :: Parser ListLines
pLines = ListLines <$> (seeNext 50 >> char '-' >> seeNext 50 >> pLine) `sepEndBy1` (spaces' >> char '\n')

