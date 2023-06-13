module Test.PathToC_Test where

import Search.Search
import Test.Hspec
import Algebra.PolyRings
import Algebra.Fourier
import Compile.PathToC

path2c_spec :: Spec
path2c_spec = do
  describe "compile Path to C" $ do
    it "compile phi kernels to CAST" $ do
      path <- turtles (Base 16 0 16 17) (Factor 2)
      compilePathToC path `shouldReturn` ""

    it "compile phi kernels to CAST" $ do
      path <- turtles (Base 8 0 8 17) (Factor 2)
      compilePathToC path `shouldReturn` ""
    
