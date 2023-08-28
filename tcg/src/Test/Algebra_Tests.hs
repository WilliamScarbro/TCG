import Test.NTT_Test
import Test.Fourier_Test
import Test.BetterAlgebra_Test

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "NTT test" ntt_spec
  describe "Fourier test" fourier_spec
  describe "BetterAlgebra test" balg_spec
