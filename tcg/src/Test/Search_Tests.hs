import Test.Hspec
import Test.Decompose_Test
import Test.EquivalenceLibrary_Test

main :: IO ()
main = hspec $ do
  describe "Decompose Tests" decomp_spec
  describe "EquivalenceLib Tests" eqlib_spec
