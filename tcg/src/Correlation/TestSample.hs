module Correlation.TestSample where

import Search.Search
import Util.KernelTimer
import Search.Decompose
import Util.Logger
import Compile.Compilers
import Compile.OptimizeIR

import Control.Monad
import System.Random

-- returns size*size paths 
random_sample_from_seiler :: Int -> Int -> IO [Path]
random_sample_from_seiler log_n size =
  do
    sp <- seiler_path log_n :: IO Path
    expansions <- sequence (do -- []
      i <- [1..size] :: [Int]
      return (ex_help size (mkStdGen $ 123*i) (decompose_expand_hillclimbing) (10,lib_empty 10) [] sp) :: [IO [Path]] ) :: IO [[Path]]
      
    return . join $ expansions :: IO [Path]
    --logObj "length of expansions" (length expansions, length . nub $ expansions)
    --unique_expansions <- return . nub $ expansions
    --shuffled_expansions <- return (shuffle' unique_expansions (length unique_expansions) (mkStdGen 10))
    --return . take size $ shuffled_expansions
  where
    -- selects random path from available
    ex_help :: Int -> StdGen -> (a -> Path -> IO (a,[Path])) -> a -> [Path] -> Path -> IO [Path]
    ex_help 0 gen expand_func aux_data sample path = return (sample++[path])
    ex_help depth gen expand_func aux_data sample path =
      do --IO
        logObj "ex_help depth " depth
        (new_aux,new_paths) <- expand_func aux_data path -- :: IO (a,[Path])
        (new_path,new_gen) <- return (randomElement gen new_paths)
        ex_help (depth-1) new_gen expand_func new_aux (sample++[path]) new_path


    randomElement :: StdGen -> [a] -> (a, StdGen)
    randomElement gen xs = let (index, newGen) = randomR (0, length xs - 1) gen
                       in (xs !! index, newGen)

forward_vs_multiply :: [Path] -> IO ([Float],[Float])
forward_vs_multiply sample =
  do
    forward_times <- sequence (fmap (\p -> timePath p "Gen") sample) :: IO [Float]
    multiply_times <- sequence (fmap (\p -> timeMultiplyPath p "Gen") sample) :: IO [Float]
    return (forward_times,multiply_times) :: IO ([Float],[Float])

id_vs_replace_swapjoinprod :: [Path] -> IO ([Float],[Float])
id_vs_replace_swapjoinprod sample =
  do
    id_times <- sequence (fmap (\p -> timePath p "Gen") sample) :: IO [Float]
    replaced_sample <- return (fmap replace_swapjoinprod sample) :: IO [Path]
    replaced_times <- sequence (fmap (\p -> timePath p "Gen") replaced_sample) :: IO [Float]
    return (id_times,replaced_times) :: IO ([Float],[Float])


print_comparison :: ([Float],[Float]) -> IO ()
print_comparison (seta,setb) =
  do
    putStr . show $ seta
    putStr "\n"
    putStr . show $ setb
    putStr "\n"
