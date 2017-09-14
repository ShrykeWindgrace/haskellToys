module Main where
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import GHC.IO.Encoding




main :: IO ()
main = do
    code_ <- getLocaleEncoding
    print code_
--     newCode_ <- mkTextEncoding "UTF16//ROUNDTRIP"
--     putStrLn "switching to"
--     print newCode_
--     setLocaleEncoding newCode_
    hspecWith defaultConfig {configFormatter = Just progress} Spec.spec