module Test.Fourier_Test where

import Algebra.FField
import Algebra.NTT
import Algebra.Fourier
import Algebra.PolyRings
import Search.Search
import Util.Util
import Test.Util
import Util.Logger

import System.Random
import Test.Hspec
 
fourier_spec :: Spec
fourier_spec = do
  describe "Fourier - decompositions of the fourier algorithm in LinearOp form" $ do
    it "turtles (Base 8 0 8 17) (Factor 2)" $
      let compare = do -- IO
            path <- turtles (Base 8 0 8 17) (Factor 2)
            (res,cor) <- maybeToIO "failed testForwardPath" (testForwardPath path)
            return (res==cor) in
      compare `shouldReturn` True
    it "turtles (Base 16 0 16 17) (Factor 2)" $
      let compare = do -- IO
            path <- turtles (Base 16 0 16 17) (Factor 2)
            (res,cor) <- maybeToIO "failed testForwardPath" (testForwardPath path)
            return (res==cor) in
      compare `shouldReturn` True
    it "turtles (Base 16 0 16 17) (Factor 4)" $
      let compare = do -- IO
            path <- turtles (Base 16 0 16 17) (Factor 4)
            (res,cor) <- maybeToIO "failed testForwardPath" (testForwardPath path)
            return (res==cor) in
      compare `shouldReturn` True
    it "Spiral 6-way size 6" $
      let path = Path (Base 6 0 6 7) [(Label 2),
                                      (Repeat 2 (Factor 3)),
                                      SwapQP,
                                      (Extend 3 (Define)),
                                      (Extend 3 (Norm)),
                                      (Extend 3 (Repeat 1 (Factor 2))),
                                      (Extend 3 SwapQP),
                                      (Extend 3 (Extend 2 Define))]
          compare = do { -- Maybe
            (res,cor) <- testForwardPath path;
            return (res==cor) } in
      compare  `shouldBe` Just True
    it "Spiral 6-way size 12" $
      let path = spiral_path12
          compare = do { -- Maybe
            (res,cor) <- testForwardPath path;
            return (res==cor) } in
      compare  `shouldBe` Just True
    it "Spiral 6-way size 6, negative-wrapped" $
      let path = spiral_path6_nw
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True

    it "Pop size 64 samples 10" $
      (testSample (Base 64 64 128 257) (mkStdGen 10) 10) `shouldReturn` True
    it "Pop size 16 samples 10" $
      (testSample (Base 16 16 128 257) (mkStdGen 10) 10) `shouldReturn` True
    it "Pop size 8 samples 100" $
      (testSample (Base 4 4 8 17) (mkStdGen 10) 100) `shouldReturn` True
    
          
      

testSample :: Ring -> StdGen -> Int -> IO Bool
testSample ring gen size =
  do
    paths <- randomSample ring gen size -- [Path]
    tested_maybe <- return (fmap test_func paths) -- [Myabe (([Int],[Int]),Path)]
    tested <- maybeToIO "Failed get testForwardPath" (sequence tested_maybe) -- [(([Int],[Int]),Path)]
    compared <- sequence (fmap compare_func tested) -- [Bool]
    compare <- return . all id $ compared
    return compare
  where
    test_func :: Path -> Maybe (([Int],[Int]),Path)
    test_func path = do
      res_cor <- testForwardPath path
      return (res_cor,path)
    compare_func :: (Eq a,Show b) => ((a,a),b) -> IO Bool
    compare_func ((res,cor),p) =
      if res==cor then
        return True
      else
        logObj "Failed LO rep of path" p >> return False
                                   
-- this only works when end ring contains all n Nth roots, so Normalize will likely break this
testForwardPath :: Path -> Maybe ([Int],[Int])
testForwardPath path =
  let
    start = path_get_start path
    prime = get_prime start
    size = get_size start in
  do -- Maybe
    lo_list <- path_define path
    result <- apply_lo_list (cannon_ffVec size prime) lo_list
    resultList <- toIntList result
    permCor <- permCor path
    return (resultList,permCor)
       
