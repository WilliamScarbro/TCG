module Search.CodeGen where

import Algebra.FField
import Algebra.NTT
import Compile.CompileKernel
import Search.Search
import Algebra.Fourier
import Algebra.PolyRings
--import PolyMult
import Data.List
import Text.Regex.Posix

import System.Process
import System.Environment

--

kt_home = (getEnv "POLYMULT_HOME") >>= (\x -> return (x++"/kernel-timer/"))
kt_relative_path :: String -> IO String
kt_relative_path str = kt_home >>= (\x -> return (x++str))

code_path :: String -> IO String
code_path fname = kt_relative_path ("src/gen/"++fname++".c")

binary_path :: String -> IO String
binary_path fname = kt_relative_path ("bin/"++fname)

--


--writeSample :: Ring -> Int -> FilePath -> IO ()
--writeSample start seed fname = code_path fname >>= (\x -> writeFile x (sampleCode start seed))

timeCode :: FilePath -> IO String
timeCode fname = do { -- IO 
  timer_path <- kt_relative_path "timer.sh";
  bin_path <- binary_path fname;
  readProcess "bash" [timer_path,bin_path] "" }

extractTimes :: String -> [Float]
extractTimes s = let (before,match,after,_) = s =~ "[0-9]+\\.[0-9]*" :: (String,String,String,[String]) in
  if match == "" then [] else [read match :: Float] ++ (extractTimes after) 

timeCodeAvg :: FilePath -> IO Float
timeCodeAvg fname = 
      do { str_res <- timeCode fname;
           res <- return (extractTimes str_res);
           sum <- return (foldr (+) 0 res);
           total <- return (length res);
           return (sum/(fromIntegral total)) }

 -- let str_res = timeCode fname in -- IO String
 -- let res = str_res >>= extractTimes in -- IO [Float] 
 --   let sum=fmap (foldr (+) 0 res) in -- IO Float
 --     sum >>= (\s -> s/(fmap length res))
 
testCode :: FilePath -> IO String
testCode fname = do { -- IO 
  tester_path <- kt_relative_path "tester.sh";
  bin_path <- binary_path fname;
  readProcess "bash" [tester_path,bin_path] "" }

--

--timeSample start seed fname = writeSample start seed fname >> timeCode fname

--testSample start seed fname = writeSample start seed fname >> testCode fname
--find_bad start fname range = fmap (filter (\x -> (isInfixOf "fail" (x!!1)))) (traverse id [traverse id [pure (show i),timeSample start i fname] | i <- range])

---

pathCode :: Path -> String
pathCode path = let pstart = path_get_start path in
  let sn = get_size pstart in
    let sb = get_root pstart in
      let sp = get_prime pstart in
        let msteps = path_get_steps path in
          squashMaybeString (msteps >>= (\steps -> Just (compile (sn,sb,sp) "Gen" steps) ) ) "Compilation Error"
       
writePath :: Path -> FilePath -> IO ()
writePath path fname = let pcode = pathCode path in
  code_path fname >>= (\x -> writeFile x pcode)
--writePath Nothing fname = code_path fname >>= (\x -> writeFile x "Compilation Error")

timePath :: Path -> FilePath -> IO Float
timePath path fname = writePath path fname >> timeCodeAvg fname

--

-- check correct
expandTerminal :: Ring -> Maybe [Int]
expandTerminal (Prod n k f) = fmap (foldr (++) []) (traverse id [(f i) >>= expandTerminal | i<- [0..k-1]])
expandTerminal (Base n d b p) = Just [d]

terminalToPerm :: Int -> Int -> Int -> [Int] -> [Int]
terminalToPerm n d b term = fmap (\x -> (x-(d `div` n)) `div` (b `div` n)) term

correctResult :: Ring -> Maybe [Int]
correctResult (Base n d b p) = let vec = ffVec n p id in
  let phi_lop = phi n n d b p in
    (mv phi_lop vec) >>= toIntList


extractResult :: FilePath -> IO [Int]
extractResult fname = do { result <- testCode fname; (\x -> pure (fmap (\num -> read num :: Int) (tail (words x)))) result }

applyPerm :: [Int] -> [Int] -> [Int]
applyPerm l perm = [l!!p | p <- perm]


checkCorrect :: Path -> FilePath -> IO (String)
checkCorrect path fname = let written = writePath path fname in
  let result = extractResult fname in
    let start = path_get_start path in
      written >> result >>= (\res -> return (squashMaybeString (do { end <- path_get_end path;
                                                          cor <- correctResult start;
                                                         term <- expandTerminal end;
                                                         perm <- return (terminalToPerm (get_size start) (get_root_power start) (get_root start) term);
                                                         permCor <- return (applyPerm cor perm);
                                                         Just (show (res==permCor)) } ) "Test error")
                                                         --Just ("Result: "++show res++"  Correct: "++show cor++"  Term:"++show term++"  Perm: "++show perm++"  PermCor: "++show permCor ) }) "Test error")
                 )
