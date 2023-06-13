module Compile.PathToC where

import Compile.FAST
import Compile.CAST
import Compile.KernelToFieldAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Search.Search
import Algebra.PolyRings

compileKersToFieldAST :: Int -> [Kernel] -> IO (Int,[String],[FieldStmt a])
compileKersToFieldAST n kers = 
 let inputVars = ["x["++show i++"]"|i<-[0..n-1]] in
  do -- IO
    lops <- maybeToIO "Path2C: Failed kernelToLO" (sequence (fmap kernelToLO kers))
    foldl foldKernelSeries (return (0,inputVars,[])) lops


compilePathToC :: Path -> IO String
compilePathToC (Path start morphs) =
  let kers = path_get_steps (Path start morphs) in
    let n = get_size start in
      do
        io_kers <- maybeToIO "Path2C: failed compileKernels" kers
        (vc,vars,stmts) <-  compileKersToFieldAST n io_kers
        simplified_fast <- return . removeOnes . removeZeros $ FieldAST stmts
        prog <- return (translateFieldToC (FField (get_prime start)) simplified_fast)
        progComplete <- return (CProgram ((initVars vc)++(get_stmts prog)++(assignResult vars)))
        return (add_boiler_plate progComplete (get_size start))
  where
    initVars :: Int -> [CStatement]
    initVars vc = [CVarDeclare CInt ("t"++show i) |i<-[0..vc-1]]
    assignResult :: [String] -> [CStatement]
    assignResult vars = fmap (\(dest,var) -> CAssignment dest (Identifier var)) [("Y["++show i++"]",vars!!i) |i<-[0..(length vars)-1]]
