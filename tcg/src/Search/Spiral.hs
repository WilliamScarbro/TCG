{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Search.Spiral where

import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey,lookup,fromList,toList)
import Control.Monad

import Search.HillClimbing
import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util
import Compile.Compilers

type ReWriteLib a = Map.Map a [[a]]

class MorphismReWriter a where
  get_rewrites :: a -> Morphism -> IO [[Morphism]]

instance MorphismReWriter (ReWriteLib Morphism) where
  get_rewrites rwlib morph =
    let
      rw_func :: Morphism -> IO [[Morphism]]
      rw_func m = case Map.lookup m rwlib of
        Just x -> return x
        Nothing -> return []
    in
      functor_rewrite rw_func morph

functor_rewrite :: (Morphism -> IO [[Morphism]]) -> Morphism -> IO [[Morphism]]
functor_rewrite rw_func morph =
  if is_leaf morph then
    rw_func morph
  else
    do -- IO
      (parent_func,child) <- maybeToIO ("Spiral: split leaf morph "++show morph) (split_parent_child morph) :: IO ((Morphism->Morphism),Morphism)
      child_rewrites <- functor_rewrite rw_func child :: IO [[Morphism]]
      mapped_child_rewrites <- return (fmap (fmap parent_func) child_rewrites) :: IO [[Morphism]]
      rewrites <- rw_func morph
      return (mapped_child_rewrites++rewrites) :: IO [[Morphism]]
      
                                                                 
spiral_expand :: MorphismReWriter a => a -> Path -> IO [Path]
spiral_expand mrw path =
  let
    morphs = path_get_morphs path :: [Morphism]
    start = path_get_start path :: Ring
  in
    join (maybeToIO "spiral_path: failed"
          ( do -- Maybe
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
            -- is_factor <- return (case morph of
            --                         Factor _ -> True
            --                        _ -> False) :: [Bool]
            is_factor <- return True
            io_morph_rewrites <- return (if is_factor
                                         then
                                           get_rewrites mrw morph
                                         else
                                           return [[morph]]
                                        ) :: [IO [[Morphism]]]
            return (do --IO
              morph_rewrites <- io_morph_rewrites :: IO [[Morphism]]
              return (replaceAt i (i+1) morph_rewrites morphs) :: IO [[Morphism]] ) :: [ IO [[Morphism]]]
        in
          fmap join (sequence rr_space)
          
-- could be done better by a function
gen_rewritelib :: Int -> (Int -> [[Morphism]]) -> ReWriteLib Morphism
gen_rewritelib n rrFactor =
  let
    factors =  non_triv_factors n
    rewrites = fmap (\factor -> (Factor factor,rrFactor factor)) factors :: [(Morphism,[[Morphism]])]
  in
    Map.fromList rewrites
 

two_step_decomp :: Int -> [[Morphism]]
two_step_decomp n =
  let
    pairs = factor_pairs n
  in
    [[Factor (fst p), Extend (fst p) (Factor (snd p)),SwapJoinProd] |p<-pairs]
  

two_step_mrw n = gen_rewritelib n two_step_decomp

spiral_expand_func :: MorphismReWriter a => a -> Path -> IO (a,[Path])
spiral_expand_func mrw path = do
  paths <- spiral_expand mrw path
  return (mrw,paths)


spiral_search :: MorphismReWriter a => SearchFunc a -> Int -> a -> Path -> IO [(Path,Float)]
spiral_search search_func depth mrw path = search_func depth spiral_expand_func compilePath mrw path
