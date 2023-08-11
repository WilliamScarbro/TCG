module Search.Decompose where



import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey,lookup,fromList,toList)
import Data.Tree
import System.Random
import Control.Monad
import Data.Maybe
import System.Random (randomRIO)

import Search.HillClimbing
import Search.Annealing
import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util
import Compile.Compilers
import Compile.OptimizeIR

find_replacements :: Int -> Ring -> Ring -> IO [[Morphism]]
find_replacements depth start end =
  fr_help depth start end []
  where
    fr_help :: Int -> Ring -> Ring -> [Morphism] -> IO [[Morphism]]
    fr_help i cur end so_far =
      if cur==end then return [so_far] else
        if i<=0 then return [] else
          if prod_dimension cur > prod_dimension end then -- prod_dimension is monotonically increasing
            return [] else
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

--type DecompLib = Map.Map (Ring,Ring) [[Morphism]]
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
  if lib_member (start,end) decompLib then
    return decompLib
    else
    do -- IO
      replacements <- find_replacements (search_depth decompLib) start end
      return $ lib_insert (start,end) replacements decompLib
      
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
decompose_path sample_size decompLib path =
  let
    morphs = path_get_morphs path :: [Morphism]
    start = path_get_start path :: Ring
    --slices = get_slices slice_len (length morphs +1) :: [(Int,Int)]
    slices = get_slices (search_depth decompLib) (length morphs +1) :: [(Int,Int)]
  in
    do -- IO
      sample_slices <- takeRandom sample_size slices :: IO [(Int,Int)]
      states <- maybeToIO "decompose_path: failed getting states" (path_get_states path) :: IO [Ring]
      state_slices <- return (fmap (\(start,end) -> (states!!start,states!!end)) sample_slices) :: IO [(Ring,Ring)]
      updated_DL <- lib_add_slices state_slices decompLib :: IO DecompLib
      alt_morphs <- get_alt_morphs updated_DL sample_slices states morphs :: IO [[Morphism]]
      alt_paths <- return (fmap (\morphs -> Path start morphs) alt_morphs) :: IO [Path]
      return (updated_DL,nub alt_paths) :: IO (DecompLib,[Path])
    where
      get_slices :: Int -> Int -> [(Int,Int)]
      get_slices max_len len =
        join [[(i,j) |j<-[i+1..(min (i+max_len) (len-1))]] | i<-[0..(len-1)]]
        
      takeRandom :: Int -> [a] -> IO [a]
      takeRandom n xs | n <= 0 = return xs
                      | otherwise = do
                          indices <- replicateM n (randomRIO (0, length xs - 1))
                          return $ map (xs !!) indices

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
                   -- a := [Morphism] => replaceAt :: [[Morphism]]
                   --logged_result <- logObj "result " result >> return result
                   return result
               ) :: [IO [[Morphism]]]
      ) :: IO [[[Morphism]]]
  in
    fmap join io_alt_morphs


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
    ds_help _ _ _ 0 path = return path
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


--decompose_path :: Int -> DecompLib -> Path -> IO (DecompLib,[Path])

decompose_expand_hillclimbing :: ExpandFunc (Int,DecompLib)
  -- (Int,DecompLib) -> Path -> IO ((Int,DecompLib),[Path])
decompose_expand_hillclimbing (slice_len,decompLib) path =
  do
    (dl,paths) <- decompose_path 100 decompLib path
    return ((slice_len,dl),paths)

    
-- an alternate version of decomp_search, built on search library functions
decompose_search :: SearchFunc (Int,DecompLib) -> Int -> (Int,DecompLib) -> Path -> IO [(Path,Float)]
decompose_search search_func depth sl_dl path = search_func depth (replace_swapjoinprod_in_expandfunc decompose_expand_hillclimbing) compilePath sl_dl path


instance AnnealingEnv DecompLib where
  update_env_with_temp temp dl = DecompLib (lib dl) (round ( temp * 4 ) + 3 ) -- assumes temp is in [0..1]
  
decompose_expand_annealing :: DecompLib -> Path -> IO (DecompLib,[Path])
decompose_expand_annealing dl path =
  decompose_path 1 dl path 

--

-- Function to randomly select an element from a list
randomChoiceIO :: [a] -> IO a
randomChoiceIO xs = do
    index <- randomRIO (0, length xs - 1)
    return $ xs !! index

-- Function to find a path from start Ring to end Ring using random search
find_replacement :: Int -> Ring -> Ring -> IO [Morphism]
find_replacement search_depth start end = _tryXTimes search_depth 1000 start end
    -- Check if start Ring and end Ring are equal, return an empty list of morphisms
  
  where
    _tryXTimes :: Int -> Int -> Ring -> Ring -> IO [Morphism]
    _tryXTimes search_depth fail_max start end
      | fail_max <= 0 = return []
      | otherwise =
        do
          possible <- _findReplacementHelper search_depth start end
          possible_end <- maybeToIO "find_replacement: failed get end" $ path_get_end (Path start possible)
          if end == possible_end then
            return possible
            else
            _tryXTimes search_depth (fail_max) start end
            
-- Helper function to recursively find a replacement
    _findReplacementHelper :: Int -> Ring -> Ring -> IO [Morphism]
    _findReplacementHelper _ start end | start == end = return []
    _findReplacementHelper 0 _ _ = return []  -- Terminate the search when search_depth is 0
    _findReplacementHelper search_depth start end = do
      -- Get a list of possible morphisms that can be applied to the start Ring
      morphisms <- morphismMatch >>= (\x -> match x start) :: IO [Morphism]
      if null morphisms
        then return []  -- If no morphisms are available, terminate the search
        else do
            -- Randomly select a morphism from the list
            selectedMorphism <- randomChoiceIO morphisms
            -- Apply the selected morphism to the start Ring to get a new Ring
            newRing <- maybeToIO "findReplacementHelper: failed apply" $ apply selectedMorphism start
            -- Recursively find the path from the new Ring to the end Ring with reduced search_depth
            remainingPath <- _findReplacementHelper (search_depth - 1) newRing end
            -- Return the selected morphism followed by the remaining path
            return (selectedMorphism : remainingPath)
