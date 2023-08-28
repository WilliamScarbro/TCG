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

import Control.Monad

compiler_name = getEnv "COMPILER"

compilePath :: Path -> IO String
compilePath path =
  let
    start = path_get_start path
    prime = get_prime start
    root_base = get_root start
  in
  do
    cname <- compiler_name
    join (maybeToIO ("COMPILER name invalid "++cname) (case cname of
       "Direct" -> return ( compiler_direct prime root_base compilePathToC_completeOpt path )
       "DirectMonty" -> return ( compiler_direct_monty prime root_base compilePathToC_completeOpt path )
       "DirectMontyInMem"-> return ( compiler_direct_monty_inmem prime root_base compilePathToC_completeOpt path )
       "Fucntional" -> return (compiler_functional path)
       "Vector" -> return (compiler_vector path)
       "VectorMonty" -> return (compiler_vector_monty path)
       _ -> Nothing ))

compileInversePath :: Path -> IO String
compileInversePath path =
  let
    start = path_get_start path
    prime = get_prime start
    root_base = get_root start
  in
  do
    cname <- compiler_name
    join (maybeToIO ("COMPILER name "++cname++" undefined for compileInversePath") (case cname of
       "Direct" -> return ( compiler_direct prime root_base compileInversePathToC_completeOpt path )
       "DirectMonty" -> return ( compiler_direct_monty prime root_base compileInversePathToC_completeOpt path )
       "DirectMontyInMem"-> return ( compiler_direct_monty_inmem prime root_base compileInversePathToC_completeOpt path )
       --"Fucntional" -> return (compiler_functional path)
       _ -> Nothing ))
      
compileMultiplyPath :: Path -> IO String
compileMultiplyPath path =
  let
    start = path_get_start path
    prime = get_prime start
    root_base = get_root start 
  in
  do
    cname <- compiler_name
    join (maybeToIO ("COMPILER name "++cname++" undefined for compileMultiplyPath") (case cname of
       "Direct" -> return ( compiler_direct_multiply prime root_base compileMultiplyPathToC_completeOpt path )
       "DirectMonty" -> return ( compiler_direct_multiply_monty prime root_base compileMultiplyPathToC_completeOpt path )
       "DirectMontyInMem" -> return ( compiler_direct_multiply_monty_inmem prime root_base compileMultiplyPathToC_completeOpt path )
       _ -> Nothing ))

compileIdentityPath :: Path -> IO String
compileIdentityPath path =
  let
    start = path_get_start path
    prime = get_prime start
    root_base = get_root start 
  in
  do
    cname <- compiler_name
    join (maybeToIO ("COMPILER name "++cname++" undefined for compileIdentityPath") (case cname of
       "Direct" -> return ( compiler_direct_identity prime root_base compileMultiplyPathToC_completeOpt path )
       "DirectMonty" -> return ( compiler_direct_identity_monty prime root_base compileMultiplyPathToC_completeOpt path )
       "DirectMontyInMem" -> return ( compiler_direct_identity_monty_inmem prime root_base compileMultiplyPathToC_completeOpt path )
       _ -> Nothing ))

-- compileInversePath :: Path -> IO String
-- compileInversePath path =
--   let
--     cfunc = _get_compile_func compileInversePathToC_completeOpt
--   in
--     cfunc path 

-- _get_field :: Field a => Path -> IO a
-- _get_field path =
--   let
--     prime = get_prime (path_get_start path)
--   in
--   do
--     cname <- compiler_name
--     field <- maybeToIO ("COMPILER name invalid "++cname) (case cname of
--        "Direct" -> Just (FField prime)
--        "DirectMonty" -> Just monty
--        "DirectMontyInMem"-> Just monty
--        _ -> Nothing )
--     return field
                                                         
-- _get_compile_func :: Field a => a -> (a -> LOClassCompileFunc a -> PruneFAST a -> BoilerPlateFunc -> Path -> IO String) -> Path -> IO String
-- _get_compile_func field compile_func path =
--   do
--     field <- _get_field path
--     cname <- compiler_name
--     cfunc <- maybeToIO ("COMPILER value invalid "++cname) (lookup cname
--       [("Direct",compile_func
--                  field
--                  compileLOPSToFieldAST
--                  pruneFast
--                  add_boiler_plate),
--         ("DirectMonty",compile_func
--                        field
--                        compileLOPSToFieldAST
--                        pruneFast
--                        add_boiler_plate_monty),
--         ("DirectMontyInMem",compile_func
--                             field
--                             compileLOPSToFieldAST_inMem
--                             pruneFast
--                             add_boiler_plate_monty),
--         ("Functional",compilePathToFunc)])     
--     cfunc path
          

compiler_direct :: Int -> Int -> (FField -> LOClassCompileFunc FField -> PruneFAST FField -> BoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct prime root_base compile_func path =
  compile_func
    (ffield_init prime root_base)
    compileLOPSToFieldAST
    pruneFast
    add_boiler_plate
    path

compiler_direct_multiply :: Int -> Int -> (FField -> LOClassCompileFunc FField -> PruneFAST FField -> MultiplyBoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_multiply prime root_base compile_func path =
  compile_func
    (ffield_init prime root_base)
    compileLOPSToFieldAST
    pruneFast
    add_boiler_plate_multiply
    path

compiler_direct_multiply_monty :: Int -> Int -> (Monty -> LOClassCompileFunc Monty -> PruneFAST Monty -> MultiplyBoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_multiply_monty prime root_base compile_func path =
  let
    monty = monty_init prime root_base
  in
    compile_func
      monty
      compileLOPSToFieldAST
      pruneFast
      add_boiler_plate_multiply_monty
      path

compiler_direct_multiply_monty_inmem :: Int -> Int -> (Monty -> LOClassCompileFunc Monty -> PruneFAST Monty -> MultiplyBoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_multiply_monty_inmem prime root_base compile_func path =
  let
    monty = monty_init prime root_base
  in
    compile_func
      monty
      compileLOPSToFieldAST_inMem
      pruneFast
      add_boiler_plate_multiply_monty
      path

compiler_direct_identity :: Int -> Int -> (FField -> LOClassCompileFunc FField -> PruneFAST FField -> MultiplyBoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_identity prime root_base compile_func path =
  compile_func
    (ffield_init prime root_base)
    compileLOPSToFieldAST
    pruneFast
    add_boiler_plate_identity
    path

compiler_direct_identity_monty :: Int -> Int -> (Monty -> LOClassCompileFunc Monty -> PruneFAST Monty -> MultiplyBoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_identity_monty prime root_base compile_func path =
  let
    monty = monty_init prime root_base
  in
    compile_func
      monty
      compileLOPSToFieldAST
      pruneFast
      add_boiler_plate_identity_monty
      path


compiler_direct_identity_monty_inmem :: Int -> Int -> (Monty -> LOClassCompileFunc Monty -> PruneFAST Monty -> MultiplyBoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_identity_monty_inmem prime root_base compile_func path =
  let
    monty = monty_init prime root_base
  in
    compile_func
      monty
      compileLOPSToFieldAST_inMem
      pruneFast
      add_boiler_plate_identity_monty
      path

compiler_direct_monty :: Int -> Int -> (Monty -> LOClassCompileFunc Monty -> PruneFAST Monty -> BoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_monty prime root_base compile_func path =
  let
    monty = monty_init prime root_base 
  in
    compile_func
      monty
      compileLOPSToFieldAST
      pruneFast
      add_boiler_plate_monty
      path

compiler_direct_monty_inmem :: Int -> Int -> (Monty -> LOClassCompileFunc Monty -> PruneFAST Monty -> BoilerPlateFunc -> Path -> IO String) -> Path -> IO String
compiler_direct_monty_inmem prime root_base compile_func path =
  let
    monty = monty_init prime root_base
  in
    compile_func
      monty
      compileLOPSToFieldAST_inMem
      pruneFast
      add_boiler_plate_monty
      path
      
compiler_functional :: Path -> IO String
compiler_functional = compilePathToFunc

compiler_vector :: Path -> IO String
compiler_vector path =
  let
    start = path_get_start path
    prime = get_prime start
    root_base = get_root start
    field = ffield_init prime root_base
  in
    compilePathToC_Vectorized field (vectorBoilerPlate field) path

compiler_vector_monty :: Path -> IO String
compiler_vector_monty path =
  let
    start = path_get_start path
    prime = get_prime start
    root_base = get_root start
    monty = monty_init prime root_base
  in
    compilePathToC_Vectorized monty (vectorBoilerPlate_Monty monty) path
--

timePath :: Path -> String -> IO Float
timePath path fname = timePath_helper path fname compilePath

timeInversePath :: Path -> String -> IO Float
timeInversePath path fname = timePath_helper path fname compileInversePath

timeMultiplyPath :: Path -> String -> IO Float
timeMultiplyPath path fname = timePath_helper path fname compileMultiplyPath

timePath_helper :: Path -> String -> (Path -> IO String) -> IO Float
timePath_helper path fname cFunc =
  do
    compiled <- cFunc path
    timed <- writeCode fname compiled >> timeCodeAvg fname
    logObj "Timed" (path,timed)
    return timed
   
