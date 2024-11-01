module Main where

import Test.Hspec

import Algebra.Algorithms
import Algebra.PolyRings

algorithm_spec :: Spec
algorithm_spec =
  let
    qqft_equiv_bitreversal l =
      it ("QFT eq BitReversal on size "++show l) $ do
        (all id $ check_eq_morph (mpm l) (base l) (qqft l) (bitreversal l)) `shouldBe` True
    qft_grouped_equiv_bitreversal l =
      it ("QFT Grouped eq BitReversal on size "++show l) $ do
        (all id $ check_eq_morph (mpm l) (base l) (qft_grouped l) (bitreversal l)) `shouldBe` True
    qqft_equiv_qft_grouped l =
      it ("QFT eq QFT Grouped on size "++show l) $ do
        (all id $ check_eq_morph (mpm l) (base l) (qqft l) (qft_grouped l)) `shouldBe` True

  in
    describe "Tests for algorithm correctness" $ do
      qqft_equiv_bitreversal 3
      qqft_equiv_bitreversal 4
      qqft_equiv_bitreversal 5

      qft_grouped_equiv_bitreversal 3
      qft_grouped_equiv_bitreversal 4
      qft_grouped_equiv_bitreversal 5
      qft_grouped_equiv_bitreversal 6
      -- qft_grouped_equiv_bitreversal 7

      qqft_equiv_qft_grouped 3
      qqft_equiv_qft_grouped 4
      qqft_equiv_qft_grouped 5
      
main :: IO ()
main = hspec $ do
  describe "Algorithm test" algorithm_spec
