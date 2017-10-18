{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module CLIOptionsLayer
  ( main'
  ) where

import           Control.Applicative (optional)
import           Control.Monad       (unless, when)
import           Data.Either         (fromRight, isRight)
import           Data.Maybe          (fromJust, isNothing)
import           Data.Semigroup      ((<>))
import           Data.Text           (pack)
-- import qualified Data.Text.IO        as DIO
-- import qualified Data.Text.Lazy      as DL
import qualified Data.Version        as DV (showVersion)
import           Lucid
import           Options.Applicative
import           Parsers.Question
import           Paths_parse4s       (version)
import           Render.Html.Rend    ()
import qualified Structures.Quest    as SQ (enumerateTours)
import           System.FilePath     ((</>))
import qualified Text.Megaparsec     as TM

data Options = Options
  { input          :: String
  , output         :: String
  , printToConsole :: Bool
  , dryRun         :: Bool
  , showVersion    :: Bool
  , withAnswers    :: Bool
  , customCSS      :: Maybe String
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
     value "out.html" <>
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
    help "show version") <*>
  switch
   (long "no-answers" <>
    help "use this if you do not want to see answers") <*>
  optional (strOption
    (long "use-custom-CSS" <> help "custom css file" <> metavar "CSS_FILE"))


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
      when printToConsole  $ do
        putStrLn ("Input file: " ++ input)
        putStrLn ("Output file: " ++ output)
        unless (isNothing customCSS) (putStr "using custom css file: " >> print (fromJust customCSS))
        cont <- readFile input
        let pd = TM.parse parseTournament "" cont
        -- print pd
        when (isRight pd) $ do
          print (fromRight undefined pd)
          renderToFile output $ qtHelper $ SQ.enumerateTours $ fromRight undefined pd
        -- DIO.putStrLn (DL.toStrict $ renderText qt)
      -- renderToFile output qt


main' :: IO ()
main' = execParser optionsH >>= mainParametrised


qtHelper :: ToHtml a => a -> Html ()
qtHelper a = html_ $
  head_ (
    meta_ [charset_ $ pack "utf8"] <>
    link_ [rel_ "stylesheet", href_ "local.css"]
  ) <>
  body_ (toHtml a)