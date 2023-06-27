import Test.Hspec
import Test.TestFourier
import Test.CAST_Test
import Test.FAST_Test
import Test.KernelToFieldAST_Test

main :: IO ()
main = hspec $ do
  describe "Define test" define_spec
  describe "CAST tests" cast_spec
  describe "FAST tests" fast_spec
  describe "K2FAST tests" ktfa_spec
