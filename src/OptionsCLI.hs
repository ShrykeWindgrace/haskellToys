module OptionsCLI 
(
    options
  , Options
  , execParser
  -- , info
  -- , helper
  -- , fullDesc
  -- , progDesc
  -- , header
  -- , (<**>)
  , input
  , output
  , dryRun
  , optionsH
)
where

import Data.Semigroup ((<>))
import Options.Applicative 
import Control.Monad

-- import  Options.Applicative


data Options = Options
  { input  :: String
  , output  :: String
  , printToConsole  :: Bool
  , dryRun  :: Bool}

options :: Parser Options
options = Options <$>
    strOption (long "input" <> short 'i' <> metavar "INPUT_FILE" <> help "path to input file" <> value "input2.txt")
    <*>
    strOption (long "input" <> short 'o' <> metavar "OUTPUT_FILE" <> help "path to output file" <> value "out2.txt")
    <*>
    switch (long "printToConsole" <> short 'p' <> help "whether to print to console the resulting output")
    <*>
    switch (long "dryRun" <> help "whether to run parser")


optionsH = info (options <**> helper)
            ( fullDesc
            <> progDesc "Parse files in 4s format"
            <> header "header" )
