module Test.TestFourier where

import Test.Hspec

import Algebra.Fourier
import Algebra.NTT
import Algebra.PolyRings

df_test_factor = define_morphism (Factor 2) (Base 8 0 8 17)
df_test_label = define_morphism (Label 2) (Base 8 0 8 17)
df_test_norm = define_morphism (Norm) (Base 8 8 16 17)
df_test_define = define_morphism (Define) (Quo 2 2 0 (Base 1 0 4 5))
--df_test_pushin = define_morphism (Pushin) (Quo 4 2 0 (Prod 2 2 (\i -> Just (Base 1 i 4 5))))


define_spec :: Spec
define_spec = do
  describe "test morphism to LinearOp" $ do
    it "(Factor 2) on (Base 8 0 8 17)" $ do
      let printed_lo = show df_test_factor
      printed_lo `shouldBe` "Just \9484                                                                                         \9488\n\9474  Just 1%17   Just 0%0   Just 0%0   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0 \9474\n\9474   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0  Just 1%17   Just 0%0   Just 0%0 \9474\n\9474   Just 0%0   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0  Just 1%17   Just 0%0 \9474\n\9474   Just 0%0   Just 0%0   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0  Just 1%17 \9474\n\9474  Just 1%17   Just 0%0   Just 0%0   Just 0%0 Just 16%17   Just 0%0   Just 0%0   Just 0%0 \9474\n\9474   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0 Just 16%17   Just 0%0   Just 0%0 \9474\n\9474   Just 0%0   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0 Just 16%17   Just 0%0 \9474\n\9474   Just 0%0   Just 0%0   Just 0%0  Just 1%17   Just 0%0   Just 0%0   Just 0%0 Just 16%17 \9474\n\9492                                                                                         \9496"
