module Main where

import  Algebra.FField
import  Algebra.Fourier
import  Algebra.NTT
import  Algebra.PolyRings
import  Compile.CompileKernel
import  Search.GeneticCode
import  Search.Genetic
import  Search.Search
import  Search.Decompose
import  Search.Spiral
import  Compile.KernelToFieldAST
import  Compile.PathToC
import  Compile.Compilers
import  Compile.CAST
import  Compile.FAST
import  Compile.Monty
import  Compile.LOClasses
import  Compile.KernelToVectorFAST
import  Compile.OptimizeIR
import  Util.Util
import  Util.Logger
import  Util.KernelTimer
import  Test.PathToC_Test
import  Test.Fourier_Test
import  Test.GeneticTest
import  Test.TimeTest_Factor
import  Test.Util
import Test.KernelToFieldAST_Test 

import qualified Data.Map as Map (empty,insert,Map,member)
import Data.Tree
import System.Random

import System.Environment
import System.Exit

 
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

main :: IO ()
--main = passEnvVars >> testPathPop new_hope_params 1000
--main = passEnvVars >> factorRun new_hope_logn
--main = passEnvVars >> (\x -> geneticRun new_hope_params x 0 >> geneticRun new_hope_params x 10 >> geneticRun kyber_params x 0 >> geneticRun kyber_params x 10 ) 20
main = do
  passEnvVars
  path <- turtles (Base 32 0 32 7681) (Factor 2)
  paths <- decompose_search hill_climbing_search 10 (5,lib_empty 5) path
  path_times <- sequence (fmap (\p -> timePath p "Gen") paths)
  putStr ( show path_times ++ "\n" )


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
