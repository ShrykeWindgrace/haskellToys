module CLIOptionsLayer
(
  main'
)
where

import Data.Semigroup ((<>))
import Options.Applicative 
import Q2
import Version

data Options = Options
  { input  :: String
  , output  :: String
  , printToConsole  :: Bool
  , dryRun  :: Bool
  , showVersion :: Bool}

options :: Parser Options
options = Options <$>
    strOption (long "input" <> short 'i' <> metavar "INPUT_FILE" <> help "path to input file" <> value "input.txt" <> showDefault)
    <*>
    strOption (long "input" <> short 'o' <> metavar "OUTPUT_FILE" <> help "path to output file" <> value "out.txt" <> showDefault)
    <*>
    switch (long "printToConsole" <> short 'p' <> help "whether to print to console the resulting output")
    <*>
    switch (long "dryRun" <> short 'd' <> help "whether to run parser")
    <*>
    switch (long "version" <> short 'v' <> help "show version")

optionsH :: ParserInfo Options
optionsH = info (options <**> helper)
            ( fullDesc
            <> progDesc "Parse INPUT_FILE assuming it has 4s format, output the result in OUTPUT_FILE"
            <> header "Parse files in 4s format" )


mainParametrised :: Options -> IO()
mainParametrised opt  
  | showVersion opt = putStrLn $ "Current version is " ++ version
  | dryRun opt = putStrLn "Dry Run"
  | otherwise = 
      putStrLn ("Input file: " ++ input opt) >>
      putStrLn ("Output file: " ++ output opt) >>
      someQQ3 (printToConsole opt) (input opt) (output opt)



main' :: IO ()
main' = mainParametrised =<< execParser optionsH