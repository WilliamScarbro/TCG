{-# LANGUAGE MultiParamTypeClasses #-}

module Search.Spiral where

import Data.Map as Map
import Control.Monad

import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util

type ReWriteLib = Map.Map Morphism [[Morphism]]

class MorphismReWriter a where
  get_rewrites :: ReWriteLib -> Morphism -> IO [[Morphism]]

instance MorphismReWriter ReWriteLib where
  get_rewrites rrLIb = maybeToIO ("failed finding morphism in rrlib") Map.lookup


spiral_search :: MorphismReWriter a => a -> Path -> IO [Path]
spiral_search mrw path =
  let
    morphs = path_get_morphs path :: [Morphism]
    start = path_get_start path :: Ring
  in
    join (maybeToIO "spiral_path: failed"
          ( do -- Maybe
              --states <- path_get_states path :: Maybe [Ring]
              return
                (do -- IO
                    alt_morphs <- get_alt_morphs mrw morphs :: IO [[Morphism]]
                    alt_paths <- return (fmap (\morphs -> Path start morphs) alt_morphs) :: IO [Path]
                    return (alt_paths) :: IO ([Path])
                ) :: Maybe (IO ([Path]))
          ))
    where
      get_alt_morphs :: MorphismReWriter a => a -> [Morphism] -> IO [[Morphism]]
      get_alt_morphs mrw morphs =
        let
          rr_space = do --List
            i<- [0..(length morphs)-1]
            morph <- return (morphs !! i) :: [Morphism]
            io_morph_rewrites <- return (get_rewrites mrw) :: [IO [[Morphism]]]
            return (do --IO
              morph_rewrites <- morph_rewrites :: IO [[Morphism]]
              return (replaceAt i (i+1) morph_rewrites morphs) :: IO [[Morphism]] ) :: [ IO [[Morphism]]]
        in
          fmap join (sequence rr_space)
          
-- could be done better by a function
gen_rewritelib :: Int -> (Int -> [Morphism]) -> ReWriteLib
gen_rewritelib n rrFactor =
  let
    factors = non_triv_factors n
    rewrites = fmap (\factor -> (Factor factor,rewriteFactor)) non_triv_factors
  in
    Map.fromList(rewrites)
 
                  
