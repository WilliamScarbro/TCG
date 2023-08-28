module Test.PathToC_Test where

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

path2c_spec :: Spec
path2c_spec =
  let
    path_test compiler description path =
      it (description++", compiler: "++compiler) $ do
        setEnv "COMPILER" compiler
        logObj ("generate test "++compiler) path
        (result,cor) <- testForwardPath_gen path
        return (result == cor) `shouldReturn` True
        
    factor_path4_test compiler = path_test compiler "compile Factor 4 path" factor_path4
    factor_path8_test compiler = path_test compiler "compile Factor 8 path" factor_path8
    spiral_path6_test compiler = path_test compiler "compile spiral 6 path" spiral_path6
    spiral_path8_test compiler = path_test compiler "compile spiral 8 path" spiral_path8
    spiral_path12_test compiler = path_test compiler "compile spiral 12 path" spiral_path12

    sample_test compiler description start size =
      it (description++", compiler: "++compiler) $ do
        setEnv "COMPILER" compiler
        (testSample test_forward_path_gen start (mkStdGen 10) size) `shouldReturn` True

    sample_20_of_size_16 compiler = sample_test compiler "compile sample 20 of size 16" (Base 16 0 16 17) 20
    sample_20_of_size_32 compiler = sample_test compiler "compile sample 20 of size 32" (Base 32 0 32 257) 20

    test_suite compiler = do
      factor_path4_test compiler 
      factor_path8_test compiler 
      spiral_path6_test compiler 
      spiral_path8_test compiler 
      spiral_path12_test compiler
      sample_20_of_size_16 compiler 
      sample_20_of_size_32 compiler 
      

  in
    describe "compile Path to C and test results" $ do
      test_suite "Direct"

      test_suite "DirectMonty"

      test_suite "DirectMontyInMem"

      test_suite "Vector"
      
      test_suite "VectorMonty"

test_forward_path_gen path =
  do
    res_cor <- testForwardPath_gen path
    return (res_cor,path)
          
-- directly compares to LO path
testForwardPath_gen :: Path -> IO ([Int],[Int])
testForwardPath_gen path =
  let
    start = path_get_start path
    prime = get_prime start
    size = get_size start
    fname = "Gen"
    --ff = FField prime
    corMaybe =
      do -- Maybe
        lo_list <- path_define path
        correct <- apply_lo_list (cannon_ffVec size prime) lo_list
        toIntList correct
    corIO = maybeToIO "Failed get correct" corMaybe in
  do -- IO
    cor <- corIO
    code <- compilePath path
    result <- writeCode fname code >> extractResult fname
    return (result,cor)
