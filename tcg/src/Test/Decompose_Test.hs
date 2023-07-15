module Test.Decompose_Test where

import Search.Decompose
import Search.Search

import Test.Hspec



import Search.Search
import Algebra.PolyRings
import Algebra.Fourier
import Algebra.NTT
import Compile.PathToC
import Util.Util
import Test.Util
import Util.KernelTimer
import Util.Logger
import Compile.FAST
import Test.Hspec
import System.Random
import Control.Monad
import Compile.Compilers

import System.Environment

decomp_spec :: Spec
decomp_spec = do
  describe "decompose Paths using library" $ do
    
--_check_decomp_paths :: Path -> [Path] -> IO Bool

    it "check decomp paths  " $ do -- IO
      path <- turtles (Base 16 0 16 17) (Factor 2)
      (newDL,paths) <- decompose_path 5 (lib_empty 5) path
      _check_decomp_path path paths `shouldReturn` True

    it "chceck decomp lib factor4_decomp" $ do -- IO
      factor4_decomp <- lib_add_slice (Base 4 0 4 5,Prod 4 4 (\i -> Just (Base 1 i 4 5))) (lib_empty 10) :: IO DecompLib
      lib_check factor4_decomp `shouldBe` True

    
    it "chceck decomp lib factor2_decomp" $ do -- IO
      factor2_decomp <- lib_add_slice (Base 4 0 4 5,Prod 4 2 (\i -> Just (Prod 2 2 (\j -> Just (Base 1 (2*j+i) 4 5))))) (lib_empty 10) :: IO DecompLib
      lib_check factor2_decomp `shouldBe` True

    it "chceck decomp lib factor2_decomp" $ do -- IO
      factor_2_4_decomp <- lib_add_slices [(Base 4 0 4 5,Prod 4 4 (\i -> Just (Base 1 i 4 5))),
                                          (Base 4 0 4 5,Prod 4 2 (\i -> Just (Prod 2 2 (\j -> Just (Base 1 (2*j+i) 4 5)))))] (lib_empty 10) 
      lib_check factor_2_4_decomp `shouldBe` True

      
      
      
  
