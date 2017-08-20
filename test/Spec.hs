import           ImageLinks
import           InlineSpec
import           ImageLinkSpec
import           Test.Hspec
import           Text.Parsec

specs = hspec <$> [imSpec2, imSpec, inlineSpec]

main = sequence_ specs