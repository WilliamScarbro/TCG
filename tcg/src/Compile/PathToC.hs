module Compile.PathToC where

import Compile.FAST
import Compile.CAST
import Compile.LOClasses
import Compile.KernelToFieldAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Search.Search
import Algebra.PolyRings
import Compile.Monty
import Compile.OptimizeIR


type KernelOptFunc = [Kernel] -> [Kernel]
type LOClassOptFunc = [LOClass] -> [LOClass]
type KernelCompileFunc a = Int -> [LOClass] -> a -> PruneFAST a -> IO CProgram
type PruneFAST a = a -> FieldAST a -> FieldAST a
type BoilerPlateFunc = (CProgram -> Int -> Int -> Maybe String)


compilePathToC :: Field a =>
  a ->
  KernelOptFunc ->
  LOClassOptFunc ->
  KernelCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Path ->
  IO String
compilePathToC field  kernel_opt lo_opt compile_func prune_func boiler_plate_func path =
  let
    start = path_get_start path
    size = get_size start
    prime = get_prime start
  in
    do -- IO
      kers <- maybeToIO "Path2C: Failed path_get_steps" (path_get_steps path)
      opt_kers <- return . kernel_opt $ kers
      lops <- maybeToIO "Path2C: Failed kernelToLO" (sequence (fmap kernelToLO opt_kers))
      opt_lops <- return (lo_opt lops)
      program <- compile_func size opt_lops field prune_func
      code <- maybeToIO "Failed adding boiler plate" (boiler_plate_func program size prime)
      return code

compilePathToC_noOpt :: Field a =>
  a ->
  KernelCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Path ->
  IO String
compilePathToC_noOpt field =
  let
    kernel_opt = id --remove_identity_kernels
    lo_opt = id --fmap expand_diagonal
  in
    compilePathToC field kernel_opt lo_opt

compilePathToC_completeOpt :: Field a =>
  a ->
  KernelCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Path ->
  IO String
compilePathToC_completeOpt field =
  let
    kernel_opt = remove_identity_kernels
    lo_opt = fmap expand_diagonal
  in
    compilePathToC field kernel_opt lo_opt
  
--

pruneFast :: Field a => PruneFAST a
pruneFast field = removeNegation . (addNegation field) . removeOnes . removeZeros 

compileKersToFieldAST_inMem :: Field a => KernelCompileFunc a 
compileKersToFieldAST_inMem n lops field prune_func =
  let
    inVars =  ["X["++show i++"]"|i<-[0..n-1]]
    outVars =  ["Y["++show i++"]"|i<-[0..n-1]]
  in
  do
    adjusted_lops <- return ( adjust_lops n lops)
    (newInVars,newOutVars,stmts) <- foldl foldKernelSeries_inMem (return (inVars,outVars,[])) adjusted_lops
    fast <- return . (prune_func field) $ FieldAST stmts
    prog <- return (translateFieldToC field fast)
    return prog
  where
    adjust_lops :: Int -> [LOClass] -> [LOClass]
    adjust_lops n lops = 
      if mod (length lops) 2 == 0 then
        lops++[LOId n]
      else
        lops
    
compileKersToFieldAST :: Field a => KernelCompileFunc a
compileKersToFieldAST n lops field prune_func = 
  let inputVars = ["X["++show i++"]"|i<-[0..n-1]] in
  do -- IO
    (vc,outVars,stmts) <- foldl foldKernelSeries (return (0,inputVars,[])) lops
    fast <- return . (prune_func field) $ FieldAST stmts
    prog <- return (translateFieldToC field fast)
    progComplete <- return (CProgram ((initVars vc)++(get_stmts prog)++(assignResult outVars)))
    return progComplete
  where
    initVars :: Int -> [CStatement]
    initVars vc = [CVarDeclare CInt ("t"++show i) |i<-[0..vc-1]]
    assignResult :: [String] -> [CStatement]
    assignResult vars = fmap (\(dest,var) -> CAssignment dest (Identifier var)) [("Y["++show i++"]",vars!!i) |i<-[0..(length vars)-1]]


-- boiler plate

includes extra =
  let standard =
        [ "#include <stdio.h>",
          "#include <stdlib.h>",
          "#include \"../Util.h\"",
          "#include \"../timer.h\"" ]
      include_extra = fmap (\x -> "#include \""++x++"\"") extra in
  standard++include_extra++[""]

gen_function :: [String] -> [String] -> [String]
gen_function [] body =
  [ "void gen(int* X,int *Y){",
    concat_lines 2 body,
    "}",
    "" ]

gen_function args body =
  [ "void gen(int* X,int* Y,"++showStrTuple args++"){",
    concat_lines 2 body,
    "}",
    "" ]
  
allocate size =
  [ "int* X = malloc(sizeof(int)*"++show size++");",
    "int* Y = malloc(sizeof(int)*"++show size++");",
    "" ]

data_init size =
  [ "for(int i=0; i<"++show size++"; i++){",
    "  X[i]=i;",
    "}",
    "" ]

call_gen = "gen(X,Y);" 

time_gen :: String -> [String]
time_gen gen_call =
  [ "initialize_timer();",
    "start_timer();",
    "",
    gen_call,
    "",
    "stop_timer();",
    "printf(\"Elapsed time: %f\\n\",elapsed_time());",
    "" ]
  
fold_result size prime =
  [ "for(int i=0; i<"++show size++"; i++){",
    "  Y[i]=(((Y[i]+"++show prime++")%"++show prime++")+"++show prime++")%"++show prime++";",
    "}",
    "" ]
  
print_result size =
  [ "print_array(\"result\",Y,"++show size++");",
    "" ]

free_data =
  [ "free(X);",
    "free(Y);",
    "" ]

summary op_count =
  [ "// op count: "++show op_count ]

main_function steps = 
  [ "int main(int argc,char** argv){",
    concat_lines 2 steps,
    "}",
    "" ] 
  
add_boiler_plate :: BoilerPlateFunc -- CProgram -> Int -> Int -> Maybe String
add_boiler_plate cp size prime =
  let
    body = translateCToStr cp
    op_count = countOperations cp
    main = main_function (allocate size++
                          data_init size++
                          time_gen call_gen++
                          fold_result size prime++
                          print_result size++
                          free_data)
    code = concat_lines 0 ((includes [])++
                           (gen_function [] body)++
                           main++
                           summary op_count) in
  Just code
           

add_boiler_plate_monty :: BoilerPlateFunc -- CProgram -> Int -> Int -> Maybe String
add_boiler_plate_monty cp size prime =
  let
    body = translateCToStr cp
    op_count = countOperations cp
    monty_scale =
      [ "for(int i=0; i<"++show size++"; i++){",
        "  fromResidue(&monty,Y[i]);",
        "}",
        "" ]
    call_gen_monty = "gen(X,Y,&monty);"
  in
  do --Maybe
    monty <- monty_init prime
    monty_init_str <- return (
      [ "monty_str monty;",
        "monty_init(&monty,"++(showTuple [p monty,r monty,pPrime monty,rInv monty,round ((log (fromIntegral (r monty))) / (log 2))])++");",
        "for(int i=0; i<"++show size++"; i++){",
        "  toResidue(&monty,X[i]);",
        "}",
        "" ] )
    main <- return (main_function (allocate size++
                                   data_init size++
                                   monty_init_str++
                                   time_gen call_gen_monty++
                                   monty_scale++
                                   fold_result size prime++
                                   print_result size++
                                   free_data) )
    code <- return (
      concat_lines 0 (includes ["../Monty.h"]++
                      gen_function ["monty_str* monty"] body++
                      main++
                      summary op_count) )
    return code

concat_lines indent lines = let
    indented_lines = fmap (\x -> whitespace (2*indent)++x) lines
    concated = foldr (\sofar cur -> sofar++"\n"++cur) "" indented_lines in
  concated

whitespace length = foldr (++) "" [" "|i<-[0..length-1]] 

