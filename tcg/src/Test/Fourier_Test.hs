module Test.Fourier_Test where

import Algebra.FField
import Algebra.NTT
import Algebra.Fourier
import Algebra.PolyRings
import Search.Search
import Util.Util
import Test.Util
import Util.Logger
import Compile.OptimizeIR

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

    it "spiral 6-way size 6, true" $
      let path = true_spiral_path6
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True

    it "spiral 6-way size 6, permuted" $
      let path = permuted_spiral_path6
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True

    it "join test 1" $
      let path = join_path1
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True

    it "join test 2" $
      let path = join_path2 
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True
        
    it "join test 3" $
      let path = join_path3 
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True
        
    it "join test 4" $
      let path = join_path4 
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True

    it "join test 5" $
      let path = join_path5
          compare = do
            (res,cor) <- testForwardPath path;
            return (res==cor) in
        compare `shouldBe` Just True
        
    it "Pop size 64 samples 10" $
      (testForwardSample (Base 64 64 128 257) (mkStdGen 10) 10) `shouldReturn` True
    it "Pop size 16 samples 10" $
      (testForwardSample (Base 16 0 16 257) (mkStdGen 20) 100) `shouldReturn` True
    it "Pop size 8 samples 100" $
      (testForwardSample (Base 4 4 8 17) (mkStdGen 10) 100) `shouldReturn` True
    

    it "inverse test (Base 4 0 4 5) (Factor 4)" $
      inverse_test_morph (Base 4 0 4 5) (Factor 4) `shouldBe` Just (mId 4,mId 4) --Just (True,True)

    it "inverse test (Base 4 4 8 17) (Factor 4)" $
      inverse_test_morph (Base 4 4 8 17) (Factor 4) `shouldBe` Just (mId 4,mId 4) -- Just (True,True)

    it "inverse test (Base 4 4 16 17) (Factor 4)" $
      inverse_test_morph (Base 4 4 16 17) (Factor 4) `shouldBe` Just (mId 4,mId 4) --Just (True,True)

    it "inverse test Extend Factor" $
      inverse_test_morph (Prod 4 2 (\i -> Just (Base 2 (2+(4*i)) 8 17))) (Extend 2 (Factor 2)) `shouldBe` Just (mId 4,mId 4) --Just (True,True)

    it "inverse test Norm" $
      inverse_test_morph (Base 4 4 8 17) (Norm) `shouldBe` Just (mId 4,mId 4)

    it "inverse test Repeat Norm" $
      inverse_test_morph (Quo 4 2 0 (Base 2 4 8 17)) (Repeat 2 Norm) `shouldBe` Just (mId 4,mId 4)

    it "inverse test Label" $
      inverse_test_morph (Base 4 4 8 17) (Label 2) `shouldBe` Just (mId 4,mId 4)

    it "inverse test SwapQQ" $
      inverse_test_morph (Quo 4 2 0 (Quo 2 1 2 (Base 2 0 8 17))) (SwapQQ) `shouldBe` Just (mId 4,mId 4)

    it "inverse test SwapJoinProd" $
      inverse_test_morph (Prod 6 3 (\i -> Just (Prod 2 2 (\j -> Just (Base 1 (i+3*j) 6 13))))) (SwapJoinProd) `shouldBe` Just (mId 6,mId 6)

    it "inverse test SwapQP" $
      inverse_test_morph (Quo 6 3 0 (Prod 2 2 (\j -> Just (Base 1 (3*j) 6 13)))) (SwapQP) `shouldBe` Just (mId 6,mId 6)

    

    it "inverse sample (Base 4 4 8 17) size 10" $
      testInverseSample (Base 4 4 8 17) (mkStdGen 10) 10 `shouldReturn` True
    it "inverse sample (Base 8 8 16 17) size 10" $
      testInverseSample (Base 4 4 8 17) (mkStdGen 10) 10 `shouldReturn` True
    it "inverse sample (Base 16 16 16 17) size 10" $
      testInverseSample (Base 4 4 8 17) (mkStdGen 10) 10 `shouldReturn` True
      
      



testForwardSample :: Ring -> StdGen -> Int  -> IO Bool
testForwardSample = testSample ((maybeToIO "Failed test_forward") . test_forward)

testInverseSample :: Ring -> StdGen -> Int  -> IO Bool
testInverseSample = testSample ((maybeToIO "Failed test_inverse") . test_inverse)


--

test_forward :: Path -> Maybe (([Int],[Int]),Path)
test_forward path = do
  res_cor <- testForwardPath path
  return (res_cor,path)

testForwardPath = testForwardPath_opt replace_swapjoinprod

testForwardPath_opt :: (Path -> Path) -> Path -> Maybe ([Int],[Int])
testForwardPath_opt path_opt path =
  let
    opt_path = path_opt path
    start = path_get_start opt_path
    prime = get_prime start
    size = get_size start in
  do -- Maybe
    lo_list <- path_define path
    result <- apply_lo_list (cannon_ffVec size prime) lo_list
    resultList <- toIntList result
    permCor <- permCor path
    return (resultList,permCor)
       


--inverse_test :: Ring -> Morphism -> Maybe (Bool,Bool)
inverse_test_morph ring morph =
  let
    size = get_size ring
  in
  do
    forward <- define_morphism morph ring
    inverse <- define_morphism (MInverse morph) ring
--    tpose_inverse <- return . tpose $ inverse
    mmfi <- mm forward inverse
    mmif <- mm inverse forward
    --return (mmfi == mId size, mmif == mId size)
    return (mmfi,mmif)

test_inverse :: Path -> Maybe (([Int],[Int]),Path)
test_inverse path =
  do
    (rec_initial,initial) <-  (inverse_test_path path)
    int_rec_initial <- toIntList rec_initial
    int_initial <- toIntList initial
    return ((int_rec_initial,int_initial),path)
    
inverse_test_path path =
  let
    start = path_get_start path
    size = get_size start
    prime = get_prime start
  in
  do
    forward <- path_define path
    inverse <- path_define_inverse path
    initial <- return (cannon_ffVec size prime)
    result <- apply_lo_list initial forward
    rec_initial <- apply_lo_list result inverse
    return (rec_initial,initial)


find_inverse_divergence :: Path -> Maybe Path
find_inverse_divergence (Path start []) = return (Path start [])
find_inverse_divergence path =
  let
    start = path_get_start path
    morphs = path_get_morphs path
  in
    do
      (rec_initial,initial) <- inverse_test_path path 
      if (rec_initial==initial) then
          return path
        else
          find_inverse_divergence (Path start (take (length morphs -1) morphs)) :: Maybe Path
     
