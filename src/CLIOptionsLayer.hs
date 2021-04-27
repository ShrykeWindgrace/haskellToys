{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# language StrictData #-}


module CLIOptionsLayer (main') where


import           Control.Applicative (optional)
import           Control.Monad       (when)
-- import           Data.Either         (isRight)
-- import           Data.Maybe          (fromJust, isNothing)
import           Data.Text           (pack)
import qualified Data.Version        as DV (showVersion)
import           Lucid               (Html, ToHtml, renderToFile, html_, head_, meta_, link_, charset_,rel_, href_, body_, toHtml)
import           Options.Applicative (Parser, ParserInfo, help, long, metavar, short, showDefault,strOption,switch,info, value,helper, header, fullDesc, progDesc, execParser)
import           Parsers.Question    (parseTournament)
import           Paths_parse4s       (version)
import           Render.Html.Rend    ()
import qualified Structures.Quest    as SQ (enumerateTours)
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
defaultInputFile  = "input.4s"


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
        maybe (pure ()) (\ccss -> putStr "using custom css file: " >> print ccss) customCSS
        cont <- readFile input
        let pd = TM.parse parseTournament "" cont
        -- print pd
        case pd of
            Right pd_ -> do
                print pd_
                renderToFile output $ qtHelper $ SQ.enumerateTours pd_
            Left _ -> pure ()



main' :: IO ()
main' = execParser optionsH >>= mainParametrised


qtHelper :: ToHtml a => a -> Html ()
qtHelper a = html_ $
  head_ (
    meta_ [charset_ $ pack "utf8"] <>
    link_ [rel_ "stylesheet", href_ "local.css"]
  ) <>
  body_ (toHtml a)
