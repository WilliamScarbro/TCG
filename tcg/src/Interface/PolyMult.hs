module Interface.PolyMult where

import Algebra.NTT
import Compile.CompileKernel
import Search.Search
import Algebra.Fourier
import Algebra.PolyRings
import qualified Data.Map as Map (empty,insert,Map,member)
import Data.Tree
import System.Random


--interface functions
pathTree :: Ring -> IO ()
pathTree r = do {
  pf <- buildPathForest r;
  pf_str <- return (fmap (fmap show) pf);
  putStrLn (drawForest pf_str); }

ringTree :: Ring -> IO()
ringTree r = do {
 rt <- buildRingTree r;
 rt_str <- return (fmap show rt);
 putStrLn (drawTree rt_str); }

--samplePath :: Ring -> Int -> IO ()
--samplePath start seed = 
--samplePath :: Ring -> Int -> IO()
--samplePath start seed = let pf = buildPathForest start in
--  let m_walk = fst (randomWalk pf (mkStdGen seed)) in
--    let code = squashMaybeString (m_walk >>= (\mw -> Just (compile (get_size start,get_root start,get_prime start) "Sampled" mw))) "Error: compilation failure" in
--        let s_walk = squashMaybeString (m_walk >>= (\ms -> Just (show ms))) "Error: exploration failure" in
--            putStrLn ("Path:\n  "++s_walk++"\n---\nCode:\n"++code) 
--
--sampleCode :: Ring -> Int -> String
--sampleCode start seed = let pf = buildPathForest start in
--  let m_walk = fst (randomWalk pf (mkStdGen seed)) in
--     squashMaybeString (m_walk >>= (\mw -> Just (compile (get_size start,get_root start,get_prime start) "Sampled" mw))) "Error: compilation failure"
--
