module Compile.Compilers where


import Algebra.PolyRings
import Search.Search
import Compile.CompileKernel
import System.Environment
import Compile.PathToC
import Util.Logger
import Util.Util
import Util.KernelTimer
import Compile.FAST
import Compile.Monty

compiler_name = getEnv "COMPILER"

compilePath :: Path -> IO String
compilePath path =
  let
    prime = get_prime (path_get_start path)
  in
  do -- IO
    cname <- compiler_name
    monty <- maybeToIO "failed monty_init" (monty_init prime)
    cfunc <- maybeToIO ("COMPILER value invalid "++cname) (lookup cname [("Direct",compilePathToC_completeOpt
                                                                           (FField prime)
                                                                           compileKersToFieldAST
                                                                           pruneFast
                                                                           add_boiler_plate),
                                                                         ("DirectMonty",compilePathToC_completeOpt
                                                                           monty
                                                                           compileKersToFieldAST
                                                                           pruneFast
                                                                           add_boiler_plate_monty),
                                                                         ("DirectMontyInMem",compilePathToC_completeOpt
                                                                           monty
                                                                           compileKersToFieldAST_inMem
                                                                           pruneFast
                                                                           add_boiler_plate_monty),
                                                                         ("Functional",compilePathToFunc)])
    cfunc path

timePath :: Path -> String -> IO Float
timePath path fname =
  do
    compiled <- compilePath path
    timed <- writeCode fname compiled >> timeCodeAvg fname
    logged <- logObj "Timed" (path,timed) >> return timed
    return logged
    
