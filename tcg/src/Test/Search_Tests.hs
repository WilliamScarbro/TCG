import Test.Hspec
import Test.Decompose_Test

main :: IO ()
main = hspec $ do
  describe "Decompose Tests" decomp_spec
