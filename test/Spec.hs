import           ImageLinks
import           ImageLinkSpec
import           Test.Hspec
import           Text.Parsec

main = sequence (hspec <$> imSpec2) >> hspec imSpec
