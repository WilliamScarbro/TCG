{-# LANGUAGE FlexibleInstances #-}
module Test.EquivalenceLibrary_Test where

import Search.EquivalenceLibrary
import Search.Search

import qualified Data.Map as Map

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

instance Show (Morphism -> Morphism) where
  show f = show (f IdR)


instance Eq (Morphism -> Morphism) where
  (==) f1 f2 = f1 IdR == f2 IdR

-- called in Search_Tests
eqlib_spec :: Spec
eqlib_spec =
  let
    --match_morph_test :: Morphism -> [VariableMap] -> 
    match_morph_test morph symmorph variable_maps =
      do
        match_maps <- return (match_symbolic_morphism morph symmorph)
        match_maps `shouldBe` variable_maps

    match_ident_test morphs symmorphs variable_maps =
      do
        match_maps <- return (match_symbolic_identity morphs symmorphs)
        match_maps `shouldBe` variable_maps
        
    match_ident_functor_test morphs symmorphs variable_maps =
      do
        match_maps <- return (match_symbolic_identity_autofunctor morphs symmorphs)
        match_maps `shouldBe` variable_maps
  in    
  do
    describe "test equivalence library functions" $ do
      it "test symbolize_morphism case 1" $ do
        sm <- symbolize_morphism cannon_factor_map (Factor 2)
        sm `shouldBe` (SymFactor ["a"])
      it "test symbolize_morphism case 2" $ do
        sm <- symbolize_morphism cannon_factor_map (Extend 3 (Factor 2))
        sm `shouldBe` (SymExtend ["b"] (SymFactor ["a"]))
  
      it "test match_symbolic_morphism case 1" $ do
        match_morph_test (Extend 3 (Factor 2)) (SymExtend ["b"] (SymFactor ["a"])) [Map.fromList [("a",2),("b",3)]]

      it "test match_symbolic_morphism case 2" $ do
        match_morph_test (Extend 6 (Repeat 3 (Factor 2))) (SymExtend ["a","b"] (SymRepeat ["a"] (SymFactor ["b"]))) [Map.fromList [("a",3),("b",2)]]

      it "test match_symbolic_morphism case 3" $ do
        match_morph_test (Extend 6 (Repeat 3 (Factor 3))) (SymExtend ["a","b"] (SymRepeat ["a"] (SymFactor ["b"]))) []

      it "test match_symbolic_morphism case 4" $ do
        match_morph_test
          (Extend 10 (Repeat 6 (Extend 2 (Repeat 5 (Factor 3)))))
          (SymExtend ["a","b"] (SymRepeat ["a","c"] (SymExtend ["a"] (SymRepeat ["b"] (SymFactor ["c"])))))
          [Map.fromList [("a",2),("b",5),("c",3)]]

      it "test match_symbolic_identity case 1" $ do
        match_ident_test
          [Label 2, Repeat 2 (Factor 3), SwapQP, Extend 3 (Repeat 2 (Factor 3))]
          [SymLabel ["a"], SymRepeat ["a"] (SymFactor ["b"]), SymSwapQP, SymExtend ["b"] (SymRepeat ["a"] (SymFactor ["c"]))]
          [Map.fromList [("a",2),("b",3),("c",3)]]

      it "test match_symbolic_identity_autofunctor case 1" $ do
        match_ident_functor_test
          [Extend 2 (Label 2), Extend 2 (Factor 3)]
          [SymLabel ["a"],SymFactor ["b"]]
          [(\m -> Extend 2 m,Map.fromList [("a",2),("b",3)])]

      it "test match_symbolic_identity_autofunctor case 2" $ do
        match_ident_functor_test
          [Extend 2 (SwapQQ)]
          [SymExtend ["a"] SymSwapQQ] 
          [(id,Map.fromList [("a",2)])]
 
