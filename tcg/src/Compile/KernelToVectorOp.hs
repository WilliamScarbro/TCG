module Compile.KernelToVectorOp where

import Compile.LOClasses
import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger

--compileLOToFieldAST :: ([String],[String]) -> LOClass  -> IO [FieldStmt a]

data PCC a = PCC {size::Int, vals::[a]}

arr_index = String -> String -> String
arr_index arr index = arr++"["++index++"]"

compileLOToVectorOp :: (String,String) -> Int -> LOCLass -> IO (PCC,String -> [FieldStmt a])
compileLOToVectorOp (inVar,outVar) (start,end) (Diagonal n f) =
  let
    pcc = PCC n [f i|i<-[0..n-1]]
    code_func = (\pcc_name -> FieldLoop (start,end) (\index -> FieldAssignment (arr_index outVar index)
                                                      (FieldOpExpr (FMultiply (Variable (arr_index pcc_name index)) (Variable (arr_index fromVars index)))))
  in
    return (pcc,code_func)


                         

