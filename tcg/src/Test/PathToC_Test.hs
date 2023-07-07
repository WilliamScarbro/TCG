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
path2c_spec = do
  describe "compile Path to C and test results" $ do

    -- Direct
    it "compile Factor 4 path Direct" $ do -- IO
      set_env <- setEnv "COMPILER" "Direct"
      (result,cor) <- return set_env >> testForwardPath2 factor_path4
      return (result == cor) `shouldReturn` True 

    it "compile Factor 8 path  Direct" $ do -- IO
      set_env <- setEnv "COMPILER" "Direct"
      (result,cor) <- return set_env >> testForwardPath2 factor_path8
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 6 Direct" $ do
      set_env <- setEnv "COMPILER" "Direct"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path6
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 8 Direct" $ do
      set_env <- setEnv "COMPILER" "Direct"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path8
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 12 Direct" $ do
      set_env <- setEnv "COMPILER" "Direct"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path12
      return (result == cor) `shouldReturn` True    
 
    it "compile sample size 64 number 20 Direct" $ do
      set_env <- setEnv "COMPILER" "Direct"
      ((return set_env) >> (testSample (Base 64 64 128 257) (mkStdGen 10) 20)) `shouldReturn` True
    
    it "compile sample size 16 number 20 Direct" $ do
      set_env <- setEnv "COMPILER" "Direct"
      ((return set_env) >> (testSample (Base 16 16 128 257) (mkStdGen 10) 20)) `shouldReturn` True
 

    -- DirectMonty
    it "compile Factor 4 path DirectMonty" $ do -- IO
      set_env <- setEnv "COMPILER" "DirectMonty"
      (result,cor) <- return set_env >> testForwardPath2 factor_path4
      return (result == cor) `shouldReturn` True
      
    it "compile Factor 8 path  DirectMonty" $ do -- IO
      set_env <- setEnv "COMPILER" "DirectMonty"
      (result,cor) <- return set_env >> testForwardPath2 factor_path8
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 6 DirectMonty" $ do
      set_env <- setEnv "COMPILER" "DirectMonty"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path6
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 8 DirectMonty" $ do
      set_env <- setEnv "COMPILER" "DirectMonty"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path8
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 12 DirectMonty" $ do
      set_env <- setEnv "COMPILER" "DirectMonty"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path12
      return (result == cor) `shouldReturn` True    

    it "compile sample size 64 number 20 DirectMonty" $ do
      set_env <- setEnv "COMPILER" "DirectMonty"
      ((return set_env) >> (testSample (Base 64 64 128 257) (mkStdGen 10) 20)) `shouldReturn` True
    
    it "compile sample size 16 number 20 DirectMonty" $ do
      set_env <- setEnv "COMPILER" "DirectMonty"
      ((return set_env) >> (testSample (Base 16 16 128 257) (mkStdGen 10) 20)) `shouldReturn` True

  
    -- DirectMontyInMem
    it "compile Factor 4 path DirectMontyInMem" $ do -- IO
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      (result,cor) <- return set_env >> testForwardPath2 factor_path4
      return (result == cor) `shouldReturn` True
      
    it "compile Factor 8 path  DirectMontyInMem" $ do -- IO
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      (result,cor) <- return set_env >> testForwardPath2 factor_path8
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 6 DirectMontyInMem" $ do
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path6
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 8 DirectMontyInMem" $ do
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path8
      return (result == cor) `shouldReturn` True

    it "compile 6-way spiral size 12 DirectMontyInMem" $ do
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      (result,cor) <- return set_env >> testForwardPath2 spiral_path12
      return (result == cor) `shouldReturn` True    

    it "compile sample size 64 number 20 DirectMontyInMem" $ do
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      ((return set_env) >> (testSample (Base 64 64 128 257) (mkStdGen 10) 20)) `shouldReturn` True
    
    it "compile sample size 16 number 20 DirectMontyInMem" $ do
      set_env <- setEnv "COMPILER" "DirectMontyInMem"
      ((return set_env) >> (testSample (Base 16 16 128 257) (mkStdGen 10) 20)) `shouldReturn` True
  

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

---- compares to full size phi transformation
--testForwardPath :: Path -> IO ([Int],[Int])
--testForwardPath path =
--  let
--    fname = "DirGen" 
--    ff = FField (get_prime (path_get_start path)) in
--  do -- IO
--    code <- compilePathToC ff add_boiler_plate path
--    result <- writeCode fname code >> extractResult fname
--    permCor <- maybeToIO "Failed get permCor" (permCor path)
--    return (result,permCor)
--
-- directly compares to LO path
testForwardPath2 :: Path -> IO ([Int],[Int])
testForwardPath2 path =
  let
    start = path_get_start path
    prime = get_prime start
    size = get_size start
    fname = "DirGen"
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
