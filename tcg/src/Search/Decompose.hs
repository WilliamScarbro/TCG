module Search.Decompose where


import Search.Search

import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey,lookup,fromList,toList)
import Data.Tree
import System.Random
import Control.Monad
import Data.Maybe

import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util

import Compile.Compilers


find_replacements :: Int -> Ring -> Ring -> IO [[Morphism]]
find_replacements depth start end =
  fr_help depth start end []
  where
    fr_help :: Int -> Ring -> Ring -> [Morphism] -> IO [[Morphism]]
    fr_help i cur end so_far =
      if cur==end then return [so_far] else
        if i<=0 then return [] else
          do -- IO
            morphs <- morphismMatch >>= (\x -> match x cur) :: IO [Morphism]
            all_possible <- return ( do -- List
              m <- morphs :: [Morphism]
              next <- return (apply m cur) :: [Maybe Ring]
              maybe_new <- return (next >>= (\ring -> return (fr_help (i-1) ring end (so_far++[m])))) :: [Maybe (IO [[Morphism]])]
              new <- return (join (maybeToIO "FindReplacements: failed apply" maybe_new)) :: [IO [[Morphism]]]
              return new)
            possible <- fmap join (sequence all_possible) :: IO [[Morphism]]
            return possible


--

data DecompLib = DecompLib { lib :: Map.Map (Ring,Ring) [[Morphism]], search_depth :: Int } deriving Show

generate_decomp_lib :: Int -> [(Ring,Ring)] -> IO DecompLib
generate_decmop_lib depth [] = return (DecompLib Map.empty depth)
generate_decomp_lib depth ring_list =
  do -- IO
    asList <- sequence (do -- List
      (start,end) <- ring_list :: [(Ring,Ring)]
      io_replacements <- return (find_replacements depth start end) :: [IO [[Morphism]]]
      return (do -- IO
        replacements <- io_replacements :: IO [[Morphism]]
        return ((start,end),replacements) :: IO ((Ring,Ring),[[Morphism]]) ) )
    return (DecompLib (Map.fromList asList) depth)

lib_insert :: (Ring,Ring) -> [[Morphism]] -> DecompLib -> DecompLib
lib_insert slice replacements decompLib =
    DecompLib (Map.insert slice replacements (lib decompLib)) (search_depth decompLib)

lib_empty :: Int -> DecompLib
lib_empty depth = DecompLib (Map.fromList []) depth

lib_member :: (Ring,Ring) -> DecompLib -> Bool
lib_member slice decompLib = Map.member slice (lib decompLib)

lib_lookup :: (Ring,Ring) -> DecompLib -> Maybe [[Morphism]]
lib_lookup slice decompLib =
  Map.lookup slice (lib decompLib)
  
lib_add_slice :: (Ring,Ring) -> DecompLib -> IO DecompLib
lib_add_slice (start,end) decompLib  =
  do -- IO
    replacements <- find_replacements (search_depth decompLib) start end :: IO [[Morphism]]
    return (if lib_member (start,end) decompLib then
              decompLib
             else
              lib_insert (start,end) replacements decompLib)
      
lib_add_slices :: [(Ring,Ring)] -> DecompLib -> IO DecompLib
lib_add_slices slices decompLib =
      foldr (\slice dl -> dl >>= lib_add_slice slice) (return decompLib) slices

lib_check :: DecompLib -> Bool
lib_check decompLib =
  let
    dlib = lib decompLib :: Map.Map (Ring,Ring) [[Morphism]]
    lib_correct =  do -- List
      ((from,to),alt_morphs) <- Map.toList dlib
      correct <- return (fmap (\(from,to,morphs) -> (Just to) == (_apply_morph_list from morphs)) [(from,to,m) | m<-alt_morphs])
      return (all id correct)
  in
    all id lib_correct
  
--



decompose_path :: Int -> DecompLib -> Path -> IO (DecompLib,[Path])
decompose_path slice_len decompLib path =
  let
    morphs = path_get_morphs path :: [Morphism]
    start = path_get_start path :: Ring
    slices = get_slices slice_len (length morphs +1) :: [(Int,Int)]
  in
    join (maybeToIO "decompose_path: failed"
          ( do -- Maybe
              states <- path_get_states path :: Maybe [Ring]
              state_slices <- return (fmap (\(start,end) -> (states!!start,states!!end)) slices) :: Maybe [(Ring,Ring)]
              return
                (do -- IO
                    updated_DL <- lib_add_slices state_slices decompLib :: IO DecompLib
                    alt_morphs <- get_alt_morphs updated_DL slices states morphs :: IO [[Morphism]]
                    alt_paths <- return (fmap (\morphs -> Path start morphs) alt_morphs) :: IO [Path]
                    return (updated_DL,alt_paths) :: IO (DecompLib,[Path])
                ) :: Maybe (IO (DecompLib,[Path]))
          ))
     
   where
     
     get_alt_morphs :: DecompLib -> [(Int,Int)] -> [Ring] -> [Morphism] -> IO [[Morphism]]
     get_alt_morphs decompLib slices states morphs =
       let
         io_alt_morphs = sequence
           (do -- List
             (start,end) <- slices :: [(Int,Int)]
             state_slice <- return (states!!start,states!!end) :: [(Ring,Ring)]
             io_replacement <- return (maybeToIO "get_alt_morphs: missing slice" (lib_lookup state_slice decompLib)) :: [IO [[Morphism]]]
             return (do -- IO
                        replacement <- io_replacement :: IO [[Morphism]]
                        --logged_replacement <- logObj "replacing " (start,end,replacement) >> return replacement 
                        result <- return (replaceAt start end replacement morphs) :: IO [[Morphism]]
                        --logged_result <- logObj "result " result >> return result
                        return result
                    ) :: [IO [[Morphism]]]
           ) :: IO [[[Morphism]]]
       in
         fmap join io_alt_morphs  
 
     get_slices :: Int -> Int -> [(Int,Int)]
     get_slices max_len len =
       join [[(i,j) |j<-[i+1..(min (i+max_len) (len-1))]] | i<-[0..(len-1)]]
 
     --replaceAt :: Int -> Int -> [a] -> [a] -> [[a]]
     replaceAt _ _ _ [] = []
     replaceAt from to r xs =
       let
         prefix = take from xs
         suffix = drop to xs
       in
      [prefix ++ r_el ++ suffix |r_el<-r]


_check_decomp_paths :: Path -> [Path] -> IO Bool
_check_decomp_paths path alt_paths =
  do -- IO
    true_end <- maybeToIO "check_decomp: failed getting end" (path_get_end path) :: IO Ring
    ends <- sequence (fmap ((\x -> if x==(Just true_end) then return True else logObj "check_decomp: failed match" (x,true_end) >> return False) . path_get_end) alt_paths) :: IO [Bool]
    return (all id ends)
  
                                                                                                               
_apply_morph_list :: Ring -> [Morphism] -> Maybe Ring
_apply_morph_list start morphs = foldl (\cur m -> cur >>= (apply m)) (Just start) morphs


-- simple hill climbing algorithm
decomp_search :: Int -> Int -> Int -> Path -> IO Path
decomp_search search_depth slice_len iterations path =
  ds_help Map.empty (lib_empty search_depth) slice_len iterations path
  where
    ds_help :: Map.Map Path Float -> DecompLib -> Int -> Int -> Path -> IO Path
    ds_help _ _ _ 0 _ = return path
    ds_help time_lib decomp_lib slice_len i path =
      do -- IO
        (updated_decomp_lib,paths) <- decompose_path slice_len decomp_lib path :: IO (DecompLib,[Path])
        updated_time_lib <- update_time_lib paths time_lib :: IO (Map.Map Path Float)
        (new_best_path,new_best_time) <- return (get_best_path updated_time_lib) :: IO (Path,Float)
        logged_new_best_path <- logObj "DecompSearch: found new best " (new_best_time,new_best_path) >> return new_best_path
        if path==logged_new_best_path then return path else
          ds_help updated_time_lib updated_decomp_lib slice_len (i-1) new_best_path

    update_time_lib :: [Path] -> Map.Map Path Float -> IO (Map.Map Path Float)
    update_time_lib paths time_lib =
      foldr check_for_and_time (return time_lib) paths
      where
        check_for_and_time path io_time_lib =
          do -- IO 
            time_lib <- io_time_lib
            if Map.member path time_lib then return time_lib else
              timePath path "DecompGen" >>= (\time -> return (Map.insert path time time_lib))
   
    get_best_path :: Map.Map Path Float -> (Path,Float)
    get_best_path map =
      let
        as_list = Map.toList map
      in
        foldr (\(path,time) (best_path,best_time) -> if time < best_time then (path,time) else (best_path,best_time) ) (head as_list) (tail as_list)
