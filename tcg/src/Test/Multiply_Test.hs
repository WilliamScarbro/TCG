module Test.Multiply_Test where

import Algebra.FField
import Algebra.NTT
import Algebra.Fourier
import Algebra.PolyRings
import Search.Search
import Util.Util
import Test.Util
import Util.Logger
import Compile.OptimizeIR
import Compile.Compilers
import Util.KernelTimer

import System.Random
import System.Environment
import Test.Hspec


multiply_spec :: Spec
multiply_spec =
  let
    sample_identity_test compiler description start size =
      it (description++", compiler "++compiler) $ do
        setEnv "COMPILER" compiler
        logObj ("inverse test "++compiler) start
        testSample test_identity_path start (mkStdGen 10) size `shouldReturn` True

    sample_identity_case_1 compiler = sample_identity_test compiler "sample identity test (Base 8 0 8 17) size 10" (Base 8 0 8 17) 10
    sample_identity_case_2 compiler = sample_identity_test compiler "sample identity test (Base 8 8 16 17) size 10" (Base 8 8 16 17) 10
    sample_identity_case_3 compiler = sample_identity_test compiler "sample identity test (Base 8 8 32 257) size 10" (Base 8 8 32 257) 10
    sample_identity_case_4 compiler = sample_identity_test compiler "sample identity test (Base 24 24 48 97) size 10" (Base 24 24 48 97) 10

    sample_multiply_test compiler description start size =
      it (description++", compiler "++compiler) $ do
        setEnv "COMPILER" compiler
        logObj ("inverse test "++compiler) start
        testSample test_multiply_path start (mkStdGen 10) size `shouldReturn` True

    sample_multiply_case_1 compiler = sample_multiply_test compiler "sample multiply test (Base 8 0 8 17) size 10" (Base 8 0 8 17) 10
    sample_multiply_case_2 compiler = sample_multiply_test compiler "sample multiply test (Base 8 8 16 17) size 10" (Base 8 8 16 17) 10
    sample_multiply_case_3 compiler = sample_multiply_test compiler "sample multiply test (Base 8 8 32 257) size 10" (Base 8 8 32 257) 10
    sample_multiply_case_4 compiler = sample_multiply_test compiler "sample multiply test (Base 24 24 48 97) size 10" (Base 24 24 48 97) 10
    

    multiply_test_suite compiler = do
      sample_identity_case_1 compiler 
      sample_identity_case_2 compiler 
      sample_identity_case_3 compiler 
      sample_identity_case_4 compiler 
      sample_multiply_case_1 compiler 
      sample_multiply_case_2 compiler 
      sample_multiply_case_3 compiler 
      sample_multiply_case_4 compiler 
      

  in
    describe "Tests for fast multiplication and inverse kernels" $ do

      multiply_test_suite "Direct"
      multiply_test_suite "DirectMonty"
      multiply_test_suite "DirectMontyInMem"
      
    -- it "identity test: turtles (Base 8 0 8 17) (Factor 2)" $
    --   let compare = do -- IO
    --         path <- turtles (Base 8 0 8 17) (Factor 2)
    --         (res,cor) <- testIdentityPath path
    --         return (res==cor)
    --   in
    --     compare `shouldReturn` True

    -- it "identity test: sample size 10 case 1" $
    --   testSample test_identity_path (Base 8 0 8 17) (mkStdGen 11) 10 `shouldReturn` True
      
    -- it "identity test: sample size 10 case 2" $
    --   testSample test_identity_path (Base 8 8 16 17) (mkStdGen 12) 10 `shouldReturn` True

    -- it "identity test: sample size 10 case 3" $
    --   testSample test_identity_path (Base 8 8 32 257) (mkStdGen 13) 10 `shouldReturn` True

    -- it "identity test: sample size 10 case 4" $
    --   testSample test_identity_path (Base 24 24 48 97) (mkStdGen 14) 10 `shouldReturn` True
    

    -- it "multiply test: sample size 10 case 1" $
    --   testSample test_multiply_path (Base 8 0 8 17) (mkStdGen 11) 10 `shouldReturn` True
      
    -- it "multiply test: sample size 10 case 2" $
    --   testSample test_multiply_path (Base 8 8 16 17) (mkStdGen 12) 10 `shouldReturn` True

    -- it "multiply test: sample size 10 case 3" $
    --   testSample test_multiply_path (Base 8 8 32 257) (mkStdGen 13) 10 `shouldReturn` True

    -- it "multiply test: sample size 10 case 4" $
    --   testSample test_multiply_path (Base 24 24 48 97) (mkStdGen 14) 10 `shouldReturn` True

true_multiply ring =
  let
    size = get_size ring
    prime = get_prime ring
    morph = Factor size
    x = cannon_ffVec size prime
  in
  do
    forward_lo <- define_morphism morph ring
    inverse_lo <- define_morphism (MInverse morph) ring
    x_t <- mv forward_lo x
    y_t <- mv forward_lo x
    x_t_t <- mv inverse_lo x_t
    z_t <- point_mult x_t y_t
    z <- mv inverse_lo z_t
    return z


test_multiply_path path =
  do
    res_cor <- testMultiplyPath path
    return (res_cor,path)
    
-- directly compares to LO path
testMultiplyPath :: Path -> IO ([Int],[Int])
testMultiplyPath path =
  let
    start = path_get_start path
    prime = get_prime start
    size = get_size start
    fname = "DirGen"
    cor = (true_multiply start) >>= toIntList
  in
  do -- IO
    cor_io <- maybeToIO "testMultiplyPath: Failed getting cor result" cor
    code <- compileMultiplyPath path
    result <- writeCode fname code >> extractResult fname
    return (result,cor_io)

test_identity_path :: Path -> IO (([Int],[Int]),Path)
test_identity_path path =
  do
    (res,cor) <- testIdentityPath path
    return ((res,cor),path)
    
testIdentityPath :: Path -> IO ([Int],[Int])
testIdentityPath path =
  let
    start = path_get_start path
    prime = get_prime start
    size = get_size start
    fname = "DirGen"
    cor = toIntList (cannon_ffVec size prime )
  in
  do -- IO
    cor_io <- maybeToIO "testMultiplyPath: Failed getting cor result" cor
    code <- compileIdentityPath path
    result <- writeCode fname code >> extractResult fname
    return (result,cor_io)
