import           ImageLinks
import           InlineSpec
import           ImageLinkSpec
import           Test.Hspec
import           Text.Parsec

main = hspec imSpec2 >> hspec imSpec >> hspec inlineSpec