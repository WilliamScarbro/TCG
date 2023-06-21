module Util.KernelTimer where

import System.Process
import System.Environment

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
