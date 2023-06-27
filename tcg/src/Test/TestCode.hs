module Test.TestCode where

import Algebra.Fourier
import Algebra.NTT
import Algebra.PolyRings
import Algebra.FField
import Search.Search
import Util.Util
import Compile.Compilers
import Util.KernelTimer
--import Search.CodeGen 



--testCode :: Path -> FilePath -> IO Bool
--testCode path code_file =
-- let  path_start = path_get_start path
--      prime = get_prime path_start
--      size = get_size path_start
--      cannon_vec = cannon_ffVec size prime
--      compare = do -- IO
--        true_result <- maybeToIO "failed creating true result" do  -- Maybe
--          lo_path <- path_define path -- [LinearOp FF]
--          apply_lo_path cannon_vec lo_path -- Maybe Vector FF
--        result <- extractResult code_file -- [Int]
--        result_vec <- return (ffVec size prime (\i -> result!!i)) -- Vector FF
--        return (true_result == result_vec) -- IO Bool
--              
testCode :: Path -> FilePath -> IO Bool
testCode path code_file = do
  let path_start = path_get_start path
      prime = get_prime path_start
      size = get_size path_start
      cannon_vec = cannon_ffVec size prime
  compare <- do -- IO
    true_result <- maybeToIO "failed creating true result" $ do  -- Maybe
      lo_path <- path_define path -- [LinearOp FF]
      apply_lo_list cannon_vec lo_path -- Maybe Vector FF
    result <- extractResult code_file -- [Int]
    let result_vec = ffVec size prime (\i -> result!!i) -- Vector FF
    return (true_result == result_vec) -- IO Bool
  return compare
