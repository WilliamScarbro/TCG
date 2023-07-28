module Search.HillClimbing where


import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey,lookup,fromList,toList)
import Algebra.Fourier
import Util.Logger
import Util.Util
import Compile.Compilers

import Control.Monad


type ExpandFunc a = a -> Path -> IO (a,[Path])
type SearchFunc a = Int -> ExpandFunc a -> a -> Path -> IO [Path]

hill_climbing_search :: SearchFunc a --Int -> (a -> Path -> IO (a,[Path])) -> a -> Path -> IO [Path]
hill_climbing_search depth expand_func aux_data path =
  hc_help depth Map.empty expand_func aux_data [path]
  where
    hc_help ::  Int -> Map.Map Path Float -> (a -> Path -> IO (a,[Path])) -> a -> [Path] -> IO [Path]
    hc_help 0 _ _  aux_data paths = return paths
    hc_help depth time_lib expand_func aux_data paths =
      do -- IO
        path <- return . last $ paths
        (new_aux,new_paths) <- expand_func aux_data path -- :: IO (a,[Path])
        --logObj "Hill Climbing: expand" new_paths
        updated_time_lib <- update_time_lib new_paths time_lib :: IO (Map.Map Path Float)
        (new_best_path,new_best_time) <- return (get_best_path updated_time_lib) :: IO (Path,Float)
        logObj "Hill Climbing Search: found new best" (new_best_path,new_best_time)
        if path == new_best_path then return paths else
          hc_help (depth-1) updated_time_lib expand_func new_aux (paths++[new_best_path])

exhaustive_search :: SearchFunc a --Int -> (a -> Path -> IO (a,[Path])) -> a -> Path -> IO [Path]
exhaustive_search depth expand_func aux_data path =
  do -- IO
    all_paths <- ex_help depth expand_func aux_data path :: IO [Path]
    updated_time_lib <- update_time_lib all_paths Map.empty :: IO (Map.Map Path Float)
    (best,best_time) <- return (get_best_path updated_time_lib) :: IO (Path,Float)
    logObj "Exhaustive Search: found best" (best,best_time)
    return [best]
  where
    ex_help :: Int -> (a -> Path -> IO (a,[Path])) -> a -> Path -> IO [Path]
    ex_help 0 _ aux_data path = return [path]
    ex_help depth expand_func aux_data path =
      do --IO
        (new_aux,new_paths) <- expand_func aux_data path -- :: IO (a,[Path])
        expanded_paths <- sequence (do -- List
          new_path <- new_paths :: [Path]
          expanded_path <- return (ex_help (depth-1) expand_func new_aux new_path) :: [IO [Path]]
          return expanded_path) :: IO [[Path]]
        return (join expanded_paths)
        
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
