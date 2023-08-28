module Compile.PathToC where

import Compile.FAST
import Compile.CAST
import Compile.LOClasses
import Compile.KernelToFieldAST
import Algebra.NTT
import Algebra.FField
import qualified Algebra.BetterAlgebra as BA
import Util.Util
import Search.Search
import Algebra.PolyRings
import Compile.Monty
import Compile.OptimizeIR
import Compile.KernelToVectorFAST
import Util.Logger

type KernelOptFunc = [Kernel] -> [Kernel]
type LOClassOptFunc = [LOClass] -> [LOClass]
type LOClassCompileFunc a = Int -> [LOClass] -> a -> PruneFAST a -> IO CProgram
type PruneFAST a = a -> FieldAST a -> FieldAST a
type BoilerPlateFunc = (CProgram -> Int -> Int -> Int -> Maybe String)
type MultiplyBoilerPlateFunc = (CProgram -> CProgram -> Int -> Int -> Int -> Maybe String)




compilePathToC_completeOpt :: Field a =>
  a ->
  LOClassCompileFunc a ->
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

compileInversePathToC_completeOpt :: Field a =>
  a ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Path ->
  IO String
compileInversePathToC_completeOpt field =
  let
    kernel_opt = remove_identity_kernels
    lo_opt = fmap expand_diagonal
  in
    compileInversePathToC field kernel_opt lo_opt

compileMultiplyPathToC_completeOpt :: Field a =>
  a ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  MultiplyBoilerPlateFunc ->
  Path ->
  IO String
compileMultiplyPathToC_completeOpt field =
  let
    kernel_opt = remove_identity_kernels
    lo_opt = fmap expand_diagonal
  in
    compileMultiplyPathToC field kernel_opt lo_opt

  
--


compilePathToC :: Field a =>
  a ->
  KernelOptFunc ->
  LOClassOptFunc ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Path ->
  IO String
compilePathToC field kernel_opt lo_opt compile_func prune_func boiler_plate_func path =
  let
    start = path_get_start path
    size = get_size start
    prime = get_prime start
    root_base = get_root start
  in
    do -- IO
      kers <- maybeToIO "Path2C: failed path_get_steps" (path_get_steps path)
      compileKernelsToC field kernel_opt lo_opt compile_func prune_func boiler_plate_func size prime root_base kers


compileMultiplyPathToC :: Field a =>
  a ->
  KernelOptFunc ->
  LOClassOptFunc ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  MultiplyBoilerPlateFunc ->
  Path ->
  IO String
compileMultiplyPathToC field kernel_opt lo_opt compile_func prune_func boiler_plate_func path =
  let
    start = path_get_start path
    size = get_size start
    prime = get_prime start
    root_base = get_root start
  in
    do
      forward_kers <- maybeToIO "Path2C: failed path_get_steps" (path_get_steps path)
      inverse_kers <- maybeToIO "Path2C: failed path_get_inverse_steps" (path_get_inverse_steps path)
      compileKernelsToC_Multiply field kernel_opt lo_opt compile_func prune_func boiler_plate_func size prime root_base forward_kers inverse_kers

compileInversePathToC :: Field a =>
  a ->
  KernelOptFunc ->
  LOClassOptFunc ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Path ->
  IO String
compileInversePathToC field kernel_opt lo_opt compile_func prune_func boiler_plate_func path =
  let
    start = path_get_start path
    size = get_size start
    prime = get_prime start
    root_base = get_root start
  in
    do -- IO
      kers <- maybeToIO "InvPath2C: failed path_get_inv_steps" (path_get_inverse_steps path)
      compileKernelsToC field kernel_opt lo_opt compile_func prune_func boiler_plate_func size prime root_base kers


compileKernelsToC :: Field a =>
  a ->
  KernelOptFunc ->
  LOClassOptFunc ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  BoilerPlateFunc ->
  Int -> --size
  Int -> --prime
  Int -> --root
  [Kernel] ->
  IO String
compileKernelsToC field kernel_opt lo_opt compile_func prune_func boiler_plate_func size prime root_base kers =
    do -- IO
      opt_kers <- return . kernel_opt $ kers
      let mpm = field_get_mpm field
      lops <- maybeToIO "Path2C: Failed kernelToLO" (sequence (fmap (kernelToLOC mpm) opt_kers))
      opt_lops <- return (lo_opt lops)
      program <- compile_func size opt_lops field prune_func
      code <- maybeToIO "Failed adding boiler plate" (boiler_plate_func program size prime root_base)
      return code

compileKernelsToC_Multiply :: Field a =>
  a ->
  KernelOptFunc ->
  LOClassOptFunc ->
  LOClassCompileFunc a ->
  PruneFAST a ->
  MultiplyBoilerPlateFunc ->
  Int -> --size
  Int -> --prime
  Int -> --root base
  [Kernel] ->
  [Kernel] ->
  IO String
compileKernelsToC_Multiply field kernel_opt lo_opt compile_func prune_func boiler_plate_func size prime root_base forward_kers inverse_kers =
    do -- IO
      let mpm = field_get_mpm field
      forward_opt_kers <- return . kernel_opt $ forward_kers
      inverse_opt_kers <- return . reverse . kernel_opt $ inverse_kers
      forward_lops <- maybeToIO "Path2C: Failed kernelToLO (foward)" (sequence (fmap (kernelToLOC mpm) forward_opt_kers))
      inverse_lops <- maybeToIO "Path2C: Failed kernelToLO (inverse)" (sequence (fmap (kernelToLOC mpm) inverse_opt_kers))
      forward_opt_lops <- return (lo_opt forward_lops)
      inverse_opt_lops <- return (lo_opt inverse_lops)
      forward_program <- compile_func size forward_opt_lops field prune_func
      inverse_program <- compile_func size inverse_opt_lops field prune_func
     
      code <- maybeToIO "Failed adding boiler plate" (boiler_plate_func forward_program inverse_program size prime root_base)
      return code


--

pruneFast :: Field a => PruneFAST a
pruneFast field = removeNegation . (addNegation field) . removeOnes . removeZeros 

--

compileLOPSToFieldAST_inMem :: Field a => LOClassCompileFunc a 
compileLOPSToFieldAST_inMem n lops field prune_func =
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
    
compileLOPSToFieldAST :: Field a => LOClassCompileFunc a
compileLOPSToFieldAST n lops field prune_func = 
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
          "#include <omp.h>",
          "#include \"../Util.h\"",
          "#include \"../timer.h\"" ]
      include_extra = fmap (\x -> "#include \""++x++"\"") extra in
  standard++include_extra++[""]

gen_function :: String -> [String] -> [String] -> [String]
gen_function name [] body =
  [ "void "++name++"(int* X,int *Y){",
    concat_lines 2 body,
    "}",
    "" ]

gen_function name args body =
  [ "void "++name++"(int* X,int* Y,"++showStrTuple args++"){",
    concat_lines 2 body,
    "}",
    "" ]

multiply_function :: Int -> String -> String -> String -> [String]
multiply_function size name forward inverse =
  ["void "++name++"(int* X,int* X_t,int* Y,int* Y_t,int* Z,int* Z_t){",
   "  "++forward++"(X,X_t);",
   "  "++forward++"(Y,Y_t);",
   "  point_multiply("++show size++",X_t,Y_t,Z_t);",
   "  "++inverse++"(Z_t,Z);",
   "}",
   ""]
  
monty_multiply_function size name forward inverse =
  ["void "++name++"(int* X,int* X_t,int* Y,int* Y_t,int* Z,int* Z_t,monty_str* monty){",
   "  "++forward++"(X,X_t,monty);",
   "  "++forward++"(Y,Y_t,monty);",
   "  point_multiply_monty("++show size++",X_t,Y_t,Z_t,monty);",
   "  "++inverse++"(Z_t,Z,monty);",
   "}",
   ""]
  

identity_function :: Int -> String -> String -> [String]
identity_function size forward inverse =
  ["void identity(int* X,int *Y){",
   "  "++forward++"(X,Y);",
   "  "++inverse++"(Y,X);",
   "  Id("++show size++",X,Y);",
   "}",
   ""]

monty_identity_function :: Int -> String -> String -> [String]
monty_identity_function size forward inverse =
  ["void identity(int* X,int *Y,int* Z,monty_str* monty){",
   "  "++forward++"(X,Y,monty);",
   "  "++inverse++"(Y,Z,monty);",
   --"  Id("++show size++",X,Y);",
   "}",
   ""]

allocate vars size =
  let
    alloc var = "int* "++var++" = malloc(sizeof(int)*"++show size++");"
  in
    (fmap alloc vars) ++ [""]

data_init vars size =
  let
    init var =  "  "++var++"[i]=i;"
  in
    ["for(int i=0; i<"++show size++"; i++){"]++
      (fmap init vars)++
      ["}",
      "" ]

call_gen = "gen(X,Y);" 

call_polymult = "polymult(X,X_t,Y,Y_t,Z,Z_t);"

call_identity = "identity(X,Y);"

time_func :: [String] -> [String]
time_func func_call =
   ["initialize_timer();",
    "start_timer();",
    ""]++
    func_call++
   ["",
    "stop_timer();",
    "printf(\"Elapsed time: %f\\n\",elapsed_time());",
    "" ]
  
fold_result var size prime =
  [ "for(int i=0; i<"++show size++"; i++){",
    "  "++var++"[i]=((("++var++"[i]+"++show prime++")%"++show prime++")+"++show prime++")%"++show prime++";",
    "}",
    "" ]
  
print_result var size =
  [ "print_array(\"result\","++var++","++show size++");",
    "" ]

free_data vars =
  let
    free var = "free("++var++");"
  in
    (fmap free vars) ++ [""]
    
summary op_count =
  [ "// op count: "++show op_count ]

main_function steps = 
  [ "int main(int argc,char** argv){",
    concat_lines 2 steps,
    "}",
    "" ] 


monty_init_struct monty =
  [ "monty_str monty;",
    "monty_init(&monty,"++(showTuple [p monty,r monty,pPrime monty,rInv monty,round ((log (fromIntegral (r monty))) / (log 2))])++");"]
  
monty_scale_to vars size =
  let
    toResidue var = var++"[i] = toResidue(&monty,"++var++"[i]);"
  in
    [ "for(int i=0; i<"++show size++"; i++){"] ++
      fmap toResidue vars ++
    [ "}",
      "" ]
  
monty_scale_from vars size =
  let
    fromResidue var = var++"[i] = fromResidue(&monty,"++var++"[i]);"
  in
    [ "for(int i=0; i<"++show size++"; i++){"]++
      fmap fromResidue vars ++
    [ "}",
      "" ]


add_boiler_plate :: BoilerPlateFunc -- CProgram -> Int -> Int -> Maybe String
add_boiler_plate cp size prime _ =
  let
    body = translateCToStr cp
    op_count = countOperations cp
    main = main_function (allocate ["X","Y"] size++
                          data_init ["X"] size++
                          time_func [call_gen]++
                          fold_result "Y" size prime++
                          print_result "Y" size++
                          free_data ["X","Y"] )
    code = concat_lines 0 ((includes [])++
                           (gen_function "gen" [] body)++
                           main++
                           summary op_count) in
  Just code
           
add_boiler_plate_multiply :: MultiplyBoilerPlateFunc -- CProgram -> CProgram -> Int -> Int -> Maybe String
add_boiler_plate_multiply forward_cp inverse_cp size prime _ =
  let
    forward_body = translateCToStr forward_cp
    inverse_body = translateCToStr inverse_cp
    all_vars = ["X","Y","Z","X_t","Y_t","Z_t"]
    main = main_function (allocate  all_vars size++
                          data_init ["X","Y"] size++
                          time_func [call_polymult]++
                          fold_result "Z" size prime++
                          print_result "Z" size++
                          free_data all_vars)
    code = concat_lines 0 ((includes ["../Multiply.h"]++
                            gen_function "forward_path" [] forward_body++
                            gen_function "inverse_path" [] inverse_body++
                            multiply_function size "polymult" "forward_path" "inverse_path"++
                            main ))
  in
    Just code

add_boiler_plate_identity :: MultiplyBoilerPlateFunc
add_boiler_plate_identity forward_cp inverse_cp size prime _ =
  let
    forward_body = translateCToStr forward_cp
    inverse_body = translateCToStr inverse_cp
    all_vars = ["X","Y"]
    main = main_function (allocate  all_vars size++
                          data_init ["X"] size++
                          time_func [call_identity]++
                          fold_result "X" size prime++
                          print_result "X" size++
                          free_data all_vars)
    code = concat_lines 0 ((includes [] ++
                            gen_function "forward_path" [] forward_body++
                            gen_function "inverse_path" [] inverse_body++
                            identity_function size "forward_path" "inverse_path"++
                            main ))
  in
    Just code
    
--


add_boiler_plate_monty :: BoilerPlateFunc -- CProgram -> Int -> Int -> Maybe String
add_boiler_plate_monty cp size prime root_base =
  let
    body = translateCToStr cp
    op_count = countOperations cp
    call_gen_monty = "gen(X,Y,&monty);"
    monty = monty_init prime root_base
  in
  do --Maybe
    main <- return (main_function (allocate ["X","Y"] size++
                                   data_init ["X"] size++
                                   monty_init_struct monty++
                                   time_func (monty_scale_to ["X"] size++
                                              [call_gen_monty]++
                                              monty_scale_from ["Y"] size)++
                                   fold_result "Y" size prime++
                                   print_result "Y" size++
                                   free_data ["X","Y"] ) )
    code <- return (
      concat_lines 0 (includes ["../Monty.h"]++
                      gen_function "gen" ["monty_str* monty"] body++
                      main++
                      summary op_count) )
    return code

add_boiler_plate_multiply_monty :: MultiplyBoilerPlateFunc
add_boiler_plate_multiply_monty forward_cp inverse_cp size prime root_base =
  let
    forward_body = translateCToStr forward_cp
    inverse_body = translateCToStr inverse_cp
    monty = monty_init prime root_base
    call_mult_monty = "polymult(X,X_t,Y,Y_t,Z,Z_t,&monty);"
    all_vars = ["X","Y","Z","X_t","Y_t","Z_t"]
  in
  do
    main <- return (main_function (allocate all_vars size++
                                   data_init ["X","Y"] size++
                                   monty_init_struct monty++
                                   time_func (monty_scale_to ["X","Y"] size++
                                              [call_mult_monty,""]++
                                              monty_scale_from ["Z"] size)++
                                   fold_result "Z" size prime++
                                   print_result "Z" size++
                                   free_data all_vars ))
    code <- return (
      concat_lines 0 (includes ["../Monty.h","../Multiply.h"]++
                      gen_function "forward_path" ["monty_str* monty"] forward_body++
                      gen_function "inverse_path" ["monty_str* monty"] inverse_body++
                      monty_multiply_function size "polymult" "forward_path" "inverse_path"++
                      main ))
    return code

add_boiler_plate_identity_monty :: MultiplyBoilerPlateFunc
add_boiler_plate_identity_monty forward_cp inverse_cp size prime root_base =
  let
    forward_body = translateCToStr forward_cp
    inverse_body = translateCToStr inverse_cp
    monty = monty_init prime root_base
    call_identity_monty = "identity(X,Y,Z,&monty);"
    all_vars = ["X","Y","Z"]
  in
  do
    main <- return (main_function (allocate all_vars size++
                                   data_init ["X"] size++
                                   monty_init_struct monty++
                                   time_func (monty_scale_to ["X"] size++
                                              [call_identity_monty,""]++
                                              monty_scale_from ["Z"] size)++
                                   fold_result "Z" size prime++
                                   print_result "Z" size++
                                   free_data all_vars ))
    code <- return (
      concat_lines 0 (includes ["../Monty.h"]++
                      gen_function "forward_path" ["monty_str* monty"] forward_body++
                      gen_function "inverse_path" ["monty_str* monty"] inverse_body++
                      monty_identity_function size "forward_path" "inverse_path"++
                      main ))
    return code

concat_lines indent lines = let
    indented_lines = fmap (\x -> whitespace (2*indent)++x) lines
    concated = foldr (\sofar cur -> sofar++"\n"++cur) "" indented_lines in
  concated

whitespace length = foldr (++) "" [" "|i<-[0..length-1]] 

--

type VectorBoilerPlate = [String] -> [CStatement] -> [String] -> Int -> Int -> String

vectorBoilerPlate :: FField -> VectorBoilerPlate
vectorBoilerPlate ffield pccInit vectorCAST pccDestroy size prime =
  let
    all_vars = ["X","Y"]
    
    init_func = ["void init_pcc(int*** pccList_pointer, int**** pccMap_pointer){",
                 concat_lines 2 pccInit,
                 "}"]
    destroy_func = ["void dest_pcc(int** pccList,int*** pccMap){",
                   concat_lines 2 pccDestroy,
                   "}"]
    gen_func = ["void gen(int* X,int* Y,int*** pccMap){",
               concat_lines 2 (translateCToStr (CProgram vectorCAST)),
               "}"]
    gen_func_monty = ["void gen(int* X,int* Y,int*** pccMap,monty_str* monty){",
               concat_lines 2 (translateCToStr (CProgram vectorCAST)),
               "}"]

    call_init_pcc = ["int** pccList;",
                "int*** pccMap;",
                "init_pcc(&pccList,&pccMap);",
                ""]
    call_gen = ["gen(X,Y,pccMap);",""]
    call_gen_monty = ["gen(X,Y,pccMap,monty);",""]
    
    call_destroy_pcc = ["dest_pcc(pccList,pccMap);",""]

    
    main_func = main_function (allocate all_vars size++
                               data_init ["X"] size++
                               call_init_pcc++
                               time_func call_gen++
                               fold_result "Y" size prime++
                               print_result "Y" size++
                               call_destroy_pcc++
                               free_data all_vars)



    code = concat_lines 0 (includes ["../Generic.h","../LPerm.h"]++
                           init_func++
                           destroy_func++
                           gen_func++
                           main_func )
           
    
  in
    code
                   
vectorBoilerPlate_Monty :: Monty -> VectorBoilerPlate
vectorBoilerPlate_Monty monty pccInit vectorCAST pccDestroy size prime =
  let
    all_vars = ["X","Y"]
    
    init_func = ["void init_pcc(int*** pccList_pointer, int**** pccMap_pointer){",
                 concat_lines 2 pccInit,
                 "}"]
    destroy_func = ["void dest_pcc(int** pccList,int*** pccMap){",
                   concat_lines 2 pccDestroy,
                   "}"]
    gen_func_monty = ["void gen(int* X,int* Y,int*** pccMap,monty_str* monty){",
                      concat_lines 2 (translateCToStr (CProgram vectorCAST)),
                      "}"]

    call_init_pcc = ["int** pccList;",
                     "int*** pccMap;",
                     "init_pcc(&pccList,&pccMap);",
                     ""]

    call_gen_monty = ["gen(X,Y,pccMap,&monty);",""]
    
    call_destroy_pcc = ["dest_pcc(pccList,pccMap);",""]

    main_func_monty = main_function (allocate all_vars size++
                                     data_init ["X"] size++
                                     call_init_pcc++
                                     monty_init_struct monty++
                                     time_func (monty_scale_to ["X"] size++
                                                call_gen_monty++
                                                monty_scale_from ["Y"] size)++
                                     fold_result "Y" size prime++
                                     print_result "Y" size++
                                     call_destroy_pcc++
                                     free_data all_vars)
    code_monty = concat_lines 0 (includes ["../Generic.h","../LPerm.h"]++
                                 init_func++
                                 destroy_func++
                                 gen_func_monty++
                                 main_func_monty)
  in
    code_monty

compilePathToC_Vectorized :: Field a => a -> VectorBoilerPlate -> Path -> IO String
compilePathToC_Vectorized field boiler_plate path =
  let
    start = path_get_start path
    size = get_size start
    prime = get_prime start
    root_base = get_root start
    mpm = BA.init_ModPrimeMemo prime root_base
  in
    maybeToIO "Path2C vectorized: compile failed" $ do
      kernels <- path_get_steps path
      let notid_kernels = remove_identity_kernels kernels
      vectorClasses <- sequence (fmap (kernelToVectorClass mpm) notid_kernels)
      (pccInit,vectorCAST,pccDestroy) <- compileVectorClasses field size vectorClasses
      return $ boiler_plate pccInit vectorCAST pccDestroy size prime
