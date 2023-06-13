import Test.Hspec
import Test.CAST_Test
import Test.FAST_Test
import Test.KernelToFieldAST_Test
import Test.PathToC_Test

main :: IO ()
main = hspec $ do
  describe "CAST tests" cast_spec
  describe "FAST tests" fast_spec
  describe "K2FAST tests" ktfa_spec
  describe "Path2C tests" path2c_spec
  
