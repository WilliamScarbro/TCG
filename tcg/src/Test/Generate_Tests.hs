import Test.Hspec
import Test.PathToC_Test
import Test.Multiply_Test

main :: IO ()
main = hspec $ do
  describe "Path2C tests" path2c_spec
  describe "Multiply tests" multiply_spec
