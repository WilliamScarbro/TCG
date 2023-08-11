module Search.HillClimbing where


import Control.Monad.State.Strict
import System.Random

import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util
import Util.KernelTimer

--import Compile.Compilers

import Control.Monad
import System.IO.Unsafe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey,lookup,fromList,toList,update)
import qualified Rando as Rando (shuffle)

type CompileFunc = Path -> IO String
type TimeFunc = Path -> IO Float
type ExpandFunc a = a -> Path -> IO (a,[Path])
type SearchFunc a = Int -> ExpandFunc a -> CompileFunc -> a -> Path -> IO [(Path,Float)]

-- maps code (String) to time (Float) and *last* path to produce code
type TimeLib = Map.Map String (Float,Path)

hill_climbing_search :: SearchFunc a --Int -> (a -> Path -> IO (a,[Path])) -> a -> Path -> IO [Path]
hill_climbing_search test_cap expand_func compile_func aux_data path =
  do
    (start_test_cap,time_lib) <- update_time_lib test_cap compile_func [path] Map.empty 
    hc_help start_test_cap time_lib expand_func aux_data [] path
  where
    hc_help ::  Int -> TimeLib -> (a -> Path -> IO (a,[Path])) -> a -> [(Path,Float)] -> Path -> IO [(Path,Float)] 
    hc_help 0 _ _  aux_data path_times path = return path_times
    hc_help test_cap time_lib expand_func aux_data path_times path =
      do -- IO
        cur_code <- compile_func path
        (cur_path_time,_) <- maybeToIO ("hill_climbing: missing cur path time: "++(show time_lib)) (Map.lookup cur_code time_lib)
        (new_aux,new_paths) <- expand_func aux_data path -- :: IO (a,[Path])
        shuffled_new_paths <- Rando.shuffle new_paths -- random order, so last path to produce code is random
        --logObj "Hill Climbing: expand" new_paths
        (updated_test_cap,updated_time_lib) <- update_time_lib test_cap compile_func shuffled_new_paths time_lib :: IO (Int,TimeLib)
        (new_best_time,new_best_path) <- return (get_best_path updated_time_lib) :: IO (Float,Path)
        logObj ("Hill Climbing Search: (cap "++show updated_test_cap++") found new best") (new_best_path,new_best_time)
        if path == new_best_path then return (path_times++[(path,cur_path_time)]) else
          hc_help updated_test_cap updated_time_lib expand_func new_aux (path_times++[(path,cur_path_time)]) new_best_path

-- exhaustive_search :: SearchFunc a --Int -> (a -> Path -> IO (a,[Path])) -> a -> Path -> IO [Path]
-- exhaustive_search depth expand_func aux_data path =
--   do -- IO
--     all_paths <- ex_help depth expand_func aux_data path :: IO [Path]
--     updated_time_lib <- update_time_lib all_paths Map.empty :: IO (Map.Map Path Float)
--     (best,best_time) <- return (get_best_path updated_time_lib) :: IO (Path,Float)
--     logObj "Exhaustive Search: found best" (best,best_time)
--     return [best]
--   where
--     ex_help :: Int -> (a -> Path -> IO (a,[Path])) -> a -> Path -> IO [Path]
--     ex_help 0 _ aux_data path = return [path]
--     ex_help depth expand_func aux_data path =
--       do --IO
--         (new_aux,new_paths) <- expand_func aux_data path -- :: IO (a,[Path])
--         expanded_paths <- sequence (do -- List
--           new_path <- new_paths :: [Path]
--           expanded_path <- return (ex_help (depth-1) expand_func new_aux new_path) :: [IO [Path]]
--           return expanded_path) :: IO [[Path]]
--         return (join expanded_paths)
        
update_time_lib :: Int -> CompileFunc -> [Path] -> TimeLib -> IO (Int,TimeLib)
update_time_lib test_cap compile_func paths time_lib =
  foldr check_for_and_time (return (test_cap,time_lib)) paths
  where
    check_for_and_time :: Path -> IO (Int,TimeLib) -> IO (Int,TimeLib)
    check_for_and_time path io_cap_time_lib =
      do -- IO 
        (cur_test_cap,time_lib) <- io_cap_time_lib
        if cur_test_cap == 0 then
          return (cur_test_cap,time_lib)
          else
          do
            code <- compile_func path
            if Map.member code time_lib then
              return (cur_test_cap,Map.update (\(t,p) -> Just (t,path)) code time_lib)
              else
              do
                time <- timeCodeString "Gen" code
                logObj "timed" time
                return (cur_test_cap-1,Map.insert code (time,path) time_lib)
          
        --if Map.member path time_lib then return time_lib else
        --   time_func path >>= (\time -> return (Map.insert path time time_lib))

get_best_path :: TimeLib -> (Float,Path)
get_best_path map =
  let
    as_list = Map.toList map :: [(String, (Float,Path))]
  in
    foldr
      (\(_,(time,path)) (best_time,best_path) -> if time < best_time then (time,path) else (best_time,best_path) )
      (snd (head as_list))
      (tail as_list)

--
