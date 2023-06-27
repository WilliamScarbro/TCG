module Compile.PathToC where

import Compile.FAST
import Compile.CAST
import Compile.KernelToFieldAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Search.Search
import Algebra.PolyRings
import Compile.Monty

compileKersToFieldAST :: Int -> [Kernel] -> IO (Int,[String],[FieldStmt a])
compileKersToFieldAST n kers = 
 let inputVars = ["X["++show i++"]"|i<-[0..n-1]] in
  do -- IO
    lops <- maybeToIO "Path2C: Failed kernelToLO" (sequence (fmap kernelToLO kers))
    foldl foldKernelSeries (return (0,inputVars,[])) lops


compilePathToC :: Field a => a -> (CProgram -> Int -> Int -> Maybe String) -> Path -> IO String
compilePathToC field boiler_plate_func (Path start morphs) =
  let kers = path_get_steps (Path start morphs)
      n = get_size start in
        --ff = FField (get_prime start) in
      do -- IO
        --monty <- maybeToIO "Path2C: failed monty_init" (monty_init (get_prime start))
        io_kers <- maybeToIO "Path2C: failed compileKernels" kers
        (vc,vars,stmts) <-  compileKersToFieldAST n io_kers
        simplified_fast <- return . removeNegation . (addNegation field) . removeOnes . removeZeros $ FieldAST stmts
        --simplified_fast <- return . removeOnes . removeZeros $ FieldAST stmts
        --simplified_fast <- return $ FieldAST stmts
        prog <- return (translateFieldToC field simplified_fast)
        progComplete <- return (CProgram ((initVars vc)++(get_stmts prog)++(assignResult vars)))
        code <- maybeToIO "Failed adding boiler plate" (boiler_plate_func progComplete (get_size start) (get_prime start))
        return code
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
  
add_boiler_plate :: CProgram -> Int -> Int -> Maybe String
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
           

add_boiler_plate_monty :: CProgram -> Int -> Int -> Maybe String
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
        "monty_init(&monty,"++(showTuple [p monty,r monty,pPrime monty,rInv monty])++");",
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

