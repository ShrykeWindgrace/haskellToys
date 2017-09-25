{-# LANGUAGE RecordWildCards #-}

module CLIOptionsLayer
  ( main'
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative
-- import           Version
import           Control.Monad       (when)
import qualified Data.Version        as DV (showVersion)
import           Paths_parse4s       (version)
import           System.FilePath     ((</>))

data Options = Options
  { input          :: String
  , output         :: String
  , printToConsole :: Bool
  , dryRun         :: Bool
  , showVersion    :: Bool
  }

defaultInputFile :: FilePath
defaultInputFile  = "test" </> "inputs" </> "input.txt"

options :: Parser Options
options =
  Options <$>
  strOption
    (long "input" <>
     short 'i' <>
     metavar "INPUT_FILE" <>
     help "path to input file" <>
     value defaultInputFile <>
     showDefault) <*>
  strOption
    (long "input" <>
     short 'o' <>
      metavar "OUTPUT_FILE" <>
     help "path to output file" <>
     value "out.txt" <>
     showDefault) <*>
  switch
    (long "printToConsole" <>
     short 'p' <>
     help "whether to print to console the resulting output") <*>
  switch
  (long "dryRun" <>
    short 'd' <>
     help "whether to run parser") <*>
  switch
   (long "version" <>
    short 'v' <>
    help "show version")

optionsH :: ParserInfo Options
optionsH =
  info
    (helper <*> options)
    (fullDesc <>
     progDesc
       "Parse INPUT_FILE assuming it has 4s format, output the result in OUTPUT_FILE" <>
     header "Parse files in 4s format")

mainParametrised :: Options -> IO ()
mainParametrised Options{..}
  | showVersion = putStrLn $ "Current version is " ++ DV.showVersion version
  | dryRun = putStrLn "Dry Run"
  | otherwise =
    putStrLn ("Input file: " ++ input) >>
    putStrLn ("Output file: " ++ output) >>
    when printToConsole (putStrLn "TBI")

main' :: IO ()
main' = execParser optionsH >>= mainParametrised
