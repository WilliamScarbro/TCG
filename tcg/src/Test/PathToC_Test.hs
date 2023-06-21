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

import Test.Hspec
import System.Random
import Control.Monad

path2c_spec :: Spec
path2c_spec = do
  describe "compile Path to C and test results" $ do
    it "compile Factor 8 path" $ do -- IO
      (result,cor) <- testForwardPath2 factor_path8
      return (result == cor) `shouldReturn` True
    it "compile 6-way spiral size 6" $ do
      (result,cor) <- testForwardPath2 spiral_path6
      return (result == cor) `shouldReturn` True
    
    it "compile 6-way spiral size 8" $ do
      (result,cor) <- testForwardPath2 spiral_path8
      return (result == cor) `shouldReturn` True
    
    it "compile 6-way spiral size 12" $ do
      (result,cor) <- testForwardPath2 spiral_path12
      return (result == cor) `shouldReturn` True    

    it "compile sample size 64" $ do
      (testSample (Base 64 64 128 257) (mkStdGen 10) 10) `shouldReturn` True

    it "compile sample size 16" $ do
      (testSample (Base 16 16 128 257) (mkStdGen 10) 10) `shouldReturn` True


testSample :: Ring -> StdGen -> Int -> IO Bool
testSample ring gen size =
  do
    paths <- randomSample ring gen size -- [Path]
    tested <- sequence (fmap test_func paths) -- [(([Int],[Int]),Path)]
    compared <- sequence (fmap compare_func tested) -- [Bool]
    compare <- return . all id $ compared
    return compare
  where
    test_func :: Path -> IO (([Int],[Int]),Path)
    test_func path = do
      res_cor <- testForwardPath2 path
      return (res_cor,path)
    compare_func :: (Eq a,Show b) => ((a,a),b) -> IO Bool
    compare_func ((res,cor),p) =
      if res==cor then
        return True
      else
        logObj "Failed LO rep of path" p >> return False

-- compares to full size phi transformation
testForwardPath :: Path -> IO ([Int],[Int])
testForwardPath path =
  let
    fname = "DirGen" in
  do -- IO
    code <- compilePathToC path 
    result <- writeCode fname code >> extractResult fname
    permCor <- maybeToIO "Failed get permCor" (permCor path)
    return (result,permCor)

-- directly compares to LO path
testForwardPath2 :: Path -> IO ([Int],[Int])
testForwardPath2 path =
  let
    start = path_get_start path
    prime = get_prime start
    size = get_size start
    fname = "DirGen"
    corMaybe =
      do -- Maybe
        lo_list <- path_define path
        correct <- apply_lo_list (cannon_ffVec size prime) lo_list
        toIntList correct
    corIO = maybeToIO "Failed get correct" corMaybe in
  do -- IO
    cor <- corIO
    code <- compilePathToC path
    result <- writeCode fname code >> extractResult fname
    return (result,cor)
