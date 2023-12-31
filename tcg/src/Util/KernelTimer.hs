module Util.KernelTimer where

import System.Process
import System.Environment
import Text.Regex.Posix
import Util.Logger

import Data.Sort
--

kt_home = (getEnv "TCG_HOME") >>= (\x -> return (x++"/kernel-timer/"))
kt_relative_path :: String -> IO String
kt_relative_path str = kt_home >>= (\x -> return (x++str))

code_path :: String -> IO String
code_path fname = kt_relative_path ("src/gen/"++fname++".c")

binary_path :: String -> IO String
binary_path fname = kt_relative_path ("bin/"++fname)

--

testCode :: FilePath -> IO String
testCode fname = do { -- IO 
  tester_path <- kt_relative_path "tester.sh";
  bin_path <- binary_path fname;
  readProcess "bash" [tester_path,bin_path] "" }


writeCode :: String -> String -> IO ()
writeCode name code = do -- IO
  code_file <- code_path name
  writeFile code_file code
  
extractResult :: FilePath -> IO [Int]
extractResult fname = do { result <- testCode fname; (\x -> pure (fmap (\num -> read num :: Int) (tail (words x)))) result }

--


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
  do
    str_res <- timeCode fname
    res <- return (extractTimes str_res)
    --logObj "time results" res
    sorted_res <- return (sort res)
    quart_len <- return (div (length sorted_res) 4)
    res_inner_quartiles <- return (take (2*quart_len) (drop quart_len sorted_res))
    sum <- return (foldr (+) 0 res_inner_quartiles)
    return (sum/(fromIntegral (2*quart_len)))


timeCodeMedian :: FilePath -> IO Float
timeCodeMedian fname =
  do
    str_res <- timeCode fname
    res <- return (extractTimes str_res)
    logObj "time results" res
    return (median res)
  where
    median l =
      (sort l) !! (div (length l) 2)

timeCodeString :: String -> String -> IO Float
timeCodeString fname code =
  do
    writeCode fname code
    timeCodeAvg fname
