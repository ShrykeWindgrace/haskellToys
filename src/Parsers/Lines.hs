module Parsers.Lines (pLine) where

import           Parsers.Inline
import           Structures.Lines
import           Text.Parsec        hiding (Line)
import           Text.Parsec.String

pLine :: Parser Line
pLine = Line <$> (many1 oneWord)