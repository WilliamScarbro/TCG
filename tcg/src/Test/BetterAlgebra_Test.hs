module Test.BetterAlgebra_Test where

import qualified Algebra.BetterAlgebra as BA
import Algebra.NTT
import Algebra.FField
import Algebra.Fourier
import Algebra.PolyRings

import Test.Hspec

balg_spec :: Spec
balg_spec =
  let
    compare_with_lop :: Integer -> (Int -> Int -> Int) -> LinearOp FF -> Expectation
    compare_with_lop prime memo_func lop =
      let
        n = size lop
        vals = [[memo_func i j|j<-[0..n-1]] |i<-[0..n-1]]
        vals_as_lop = linearOp n ((\x -> Just (Res x prime)) . fromIntegral . uncurry memo_func)
      in
        vals_as_lop `shouldBe` lop
    diag_to_square :: (Int -> Int) -> (Int -> Int -> Int)
    diag_to_square f x y = if x==y then f x else 0
    
  in
    describe "Better Algebra: memorized implementation of finite fields" $ do
      it "phi 4 4 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (phi_func_memo mpm 4 4 0) (phi 4 4 0 4 5)  

      it "phi 4 2 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (phi_func_memo mpm 4 2 0) (phi 4 2 0 4 5)  

      it "phi_inv 4 4 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (phi_inv_func_memo mpm 4 4 0) (phi_inv 4 4 0 4 5)  

      it "phi_inv 4 2 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (phi_inv_func_memo mpm 4 2 0) (phi_inv 4 2 0 4 5)  
  
      it "gamma 4 4 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (diag_to_square $ gamma_func_memo mpm 4 4 0) (gamma 4 4 0 4 5)  

      it "gamma 4 2 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (diag_to_square $ gamma_func_memo mpm 4 2 0) (gamma 4 2 0 4 5)  

      it "gamma_inv 4 4 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (diag_to_square $ gamma_inv_func_memo mpm 4 4 0) (gamma_inv 4 4 0 4 5)  

      it "gamma_inv 4 2 0 4 5" $
        let
          mpm = BA.init_ModPrimeMemo 5 4
        in
          compare_with_lop 5 (diag_to_square $ gamma_inv_func_memo mpm 4 2 0) (gamma_inv 4 2 0 4 5)  
  
