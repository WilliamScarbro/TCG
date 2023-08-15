module Main where

import Algebra.FField
import Algebra.Fourier
import Algebra.NTT
import Algebra.PolyRings
import Compile.CompileKernel
import Search.GeneticCode
import Search.Genetic
import Search.Search
import Search.Decompose
import Search.EquivalenceLibrary
import Search.Spiral
import Search.HillClimbing
import Search.Annealing
import Compile.KernelToFieldAST
import Compile.PathToC
import Compile.Compilers
import Compile.CAST
import Compile.FAST
import Compile.Monty
import Compile.LOClasses
import Compile.KernelToVectorFAST
import Compile.OptimizeIR
import Util.Util
import Util.Logger
import Util.KernelTimer
import Util.SmallEqLib
--import Util.LargeEqLib
import Test.PathToC_Test
import Test.Fourier_Test
import Test.GeneticTest
import Test.TimeTest_Factor
import Test.Util
import Test.KernelToFieldAST_Test 
import Correlation.TestSample
import Correlation.RandomSample
import Data.Time

import qualified Data.Map as Map (empty,insert,Map,member)
import Data.Tree
import System.Random
import System.Random.Shuffle

import System.Environment
import System.Exit
import Control.Monad
import Data.List

 --test vars
path = [(Phi 4 2 0 4 5), Kernel_Repeat 4 2 (Phi 2 2 0 4 5), Kernel_Extend 4 2 (\i -> Just (Phi 2 2 (2*i) 4 5))]
 
 --interface functions
--pathTree :: Ring -> IO()
--pathTree r = putStrLn (drawForest (fmap (fmap show) (buildPathForest r)))
-- 
--ringTree :: Ring -> IO()
--ringTree r = putStrLn (drawTree (fmap show (buildRingTree r)))
-- 
--samplePath :: Ring -> Int -> IO()
--samplePath start seed = let pf = buildPathForest start in
--   let m_walk = fst (randomWalk pf (mkStdGen seed)) in
--     let code = squashMaybeString (m_walk >>= (\mw -> Just (compile (get_size start,get_root start,get_prime start) "Sampled" mw))) "Error: compilation failure" in
--         let s_walk = squashMaybeString (m_walk >>= (\ms -> Just (show ms))) "Error: exploration failure" in
--             putStrLn ("Path:\n  "++s_walk++"\n---\nCode:\n"++code) 
--

new_hope_logn = (8,12289)
kyber_params_logn = (8,7681)

passEnvVars :: IO ()
passEnvVars = let vars=["TCG_HOME","TIMER_ITERS","MATCH_CONTEXT"] in
  sequence (fmap (\v -> getEnv v >>= setEnv v >> logIO ("MAIN Set EnvVar "++v) (getEnv v)) vars) >> return ()
                   
-- params -> size -> gens
geneticRun :: (Int,Int) -> Int -> Int -> IO ()
geneticRun params size gens = (putStrLn ("-----\nGenetic Run N:"++show (fst params)++" P:"++show (snd params)++" PopSize:"++show size++" Gens:"++show gens)) >> testNthGen params size gens

factorRun :: (Int,Int) -> IO ()
factorRun (logn,p) = do { -- IO
  fp <- (timeFactorPaths logn p); -- [(Int,Int,Float)]
  putStrLn ( show fp )  }

decompose_test :: Int -> IO () 
decompose_test log_n = 
  do  
    passEnvVars 
    sp <- seiler_path log_n
    (path_times,elapsedTime) <- timeCodeBlock $ do
      decompose_search hill_climbing_search 100 (5,lib_empty 5) sp
    only_times <- return (fmap snd path_times)
    writeFile ("results/decompose_"++show log_n) ((show only_times)++"\n"
                                                  ++(show (last . fmap fst $ path_times))++"\n"
                                                  ++"Time: "++show elapsedTime++"\n")
timeCodeBlock :: IO a -> IO (a, NominalDiffTime)
timeCodeBlock action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    return (result, elapsedTime)

-- simulatedAnnealing :: forall a. AnnealingEnv a =>
--   (a -> Path -> IO (a, [Path])) ->
--   (String -> IO Float) ->
--   (Path -> IO String) ->
--   a ->
--   Path ->
--   IO Path
annealing_test :: Int -> IO ()
annealing_test log_n =
  do
    passEnvVars
    sp <- seiler_path log_n
    ((history,best),elapsedTime) <- timeCodeBlock $ do
      simulatedAnnealing decompose_expand_annealing (timeCodeString "Gen") compilePath (lib_empty 7) sp
    only_times <- return (fmap snd history)
    writeFile ("results/annealing_"++show log_n) ((show only_times)++"\n"
                                                  ++(show best)++"\n"
                                                  ++"Time: "++show elapsedTime++"\n")
build_equiv_lib :: Int -> IO ()
build_equiv_lib search_depth =
  do
    elib <- buildEquivalenceLibrary search_depth
    writeFile "results/equiv_library" (show elib++"\n"
                                      ++"-- length "++ show (length elib)++"\n")

equiv_lib_test :: EquivalenceLibrary -> Int -> IO ()
equiv_lib_test elib log_n =
  let
    n=2^log_n
    p=splitting_prime (2*n)
  in
  do
    passEnvVars
    --let path = (Path (Base n n (2*n) p) [Factor n])
    path <- seiler_path log_n
    ((history,best),elapsedTime) <- timeCodeBlock $ do
      simulatedAnnealing id_guided_expand_annealing (timeCodeString "Gen") compilePath (elib,10,1000) path
    only_times <- return (fmap snd history)
    writeFile ("results/equiv_annealing_"++show log_n) ((show only_times)++"\n"
                                                  ++(show best)++"\n"
                                                  ++"Time: "++show elapsedTime++"\n")
main :: IO () 
--main = passEnvVars >> testPathPop new_hope_params 1000 
--main = passEnvVars >> factorRun new_hope_logn 
--main = passEnvVars >> (\x -> geneticRun new_hope_params x 0 >> geneticRun new_hope_params x 10 >> geneticRun kyber_params x 0 >> geneticRun kyber_params x 10 ) 20 
--main = do
--  decompose_test 7

--main = build_equiv_lib 10
main = equiv_lib_test small_eq_lib 7

  -- main = do
--   sample <- random_sample_from_seiler 7 4
--   putStr . show $ sample
  
-- main = do 
--   sample <- return sample_size_10_base_128
--   fvm <- forward_vs_multiply sample
--   print_comparison fvm
  --decompose_test 32



-- main = do
--   passEnvVars
--   gen_paths <- sequence [randomPath (Base 16 0 16 7681) (mkStdGen i) |i<-[1000..1005]] :: IO [(Path,StdGen)]
--   paths <- return (fmap fst gen_paths) :: IO [Path]
--   opt_paths <- return (fmap (\p -> (replace_swapjoinprod p,p)) paths) :: IO [(Path,Path)]
--   timed_paths <- sequence (fmap (\(p1,p2) -> do
--                                     t1 <- timePath p1 "OptGen"
--                                     t2 <- timePath p2 "Gen"
--                                     return (t1,t2)
--                                 ) opt_paths) :: IO [(Float,Float)] 
--   putStr ("removed swap join, original "++show timed_paths++"\n")
  
 --main = putStrLn (drawForest (fmap (fmap show) (buildPathForest (Base 4 0 4 5))))
 --main = putStrLn $ drawTree $ fmap $ (fmap show) $ buildTree $ (Base 4 0 4 5)
 --main = let graph = search [Base 4 0 4 5] expand Map.empty in putStrLn (show (terminal graph))
 --main = putStrLn (show (search_morphs [Base 4 0 4 5] Map.empty))
 --main = putStrLn (compile (4,5) "Generated4" path)
 --main = putStrLn (show (fst (randomWalk (buildPathForest (Base 4 0 4 5)) (mkStdGen 100))))
--main = putStrLn (squashMaybeString ((fst (randomWalk (buildPathForest (Base 4 0 4 5)) (mkStdGen 100))) >>= (\mw -> Just (compile (4,4,5) "Sampled4" mw)))  "Error: compilation failed")
 --  
 --  code <- squashMaybeString m_code "Error: compilation failed";
 --  putStrLn (show code);
 --  }
 --
 


-- CLI
--main = getArgs >>= parse >>= putStr
--
--codeGen :: (String,Ring,Int) -> String
--codeGen = samplePath
--
--toInt :: String -> Int
--toInt s = read s :: Int
--
--parse :: [String] -> IO (String,Ring)
--parse [name,n,d,b,p,seed] = codeGen (name,Base (toInt n) (toInt d) (toInt b) (toInt p),toInt seed)
--parse ["-h"] = usage >> exit
--parse x = usage >> exit
--
--exit = exitWith ExitSuccess
--usage = putStrLn "Usage: PolyMult [flags]\n  --name : kernel name\n  --ring : start ring"
--
