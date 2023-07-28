module Compile.KernelToVectorFAST where

import Compile.LOClasses
import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger

import Control.Monad

--compileLOCToFieldAST :: ([String],[String]) -> LOClass  -> IO [FieldStmt a]

data PCC a = PCC {size::Int, vals::[a]} | EmptyPCC

-- this should be part of FieldAST
arr_index :: String -> String -> String
arr_index arr index = arr++"["++index++"]"

  -- = Diagonal Int (Int -> FF)
  --  | Permutation Int (Int -> Int)
  --  | Square Int (Int -> Int -> FF)
  --  | ITensor Int Int (LOClass) -- (LO \circledtimes I)
  --  | Partition Int [LOClass] -- m (size of each partition)
  --  | LOId Int

-- compileLOCToVectorFAST ::  (String,String) -> (Int,Int) -> LOClass -> IO (PCC FF,String -> [FieldStmt FF])
-- compileLOCToVectorFAST (inVar,outVar) (start,end) (Diagonal n f) =
--   let
--     pcc = PCC n [f i|i<-[0..n-1]]
--     assign_func pcc_name index = [FieldAssignment (arr_index outVar index)
--                                     (FieldOpExpr (FMultiply (Variable (arr_index pcc_name index)) (Variable (arr_index inVar index))))]
--     loop_func pcc_name = [FieldLoop (start,end) (assign_func pcc_name)]
--   in
--     return (pcc,loop_func)

-- -- use functional implementation
-- -- compileLOToVectorOp (inVar,outVar) (start,end) (Permutation n f) =
-- --   let
-- --     assign_func index = [FieldAssignment (arr_index outVar index) (Variable (arr_index inVar (show (f index))))]
-- --     loop_func pcc_name = [FieldLoop (start,end) assign_func]
-- --   in
-- --     (EmptyPCC,loop_func)
    
-- compileLOCToVectorFAST (inVar,outVar) (start,end) (Square n f) =
--   let
--     pcc = PCC (n*n) (join [[f i j|j<-[0..n-1]]|i<-[0..n]]) -- uses row major order
--     expr_func pcc_name i j = FieldOpExpr (FMultiply (Variable (arr_index pcc_name (show n++"*"++i++"+"++j))) (Variable (arr_index inVar j)))
--     assign_func pcc_name i j = [FieldAssignment (arr_index outVar i) (FieldOpExpr (FAdd (Variable (arr_index outVar i)) (expr_func pcc_name i j)))]
--     inner_loop_func pcc_name i = [FieldLoop (start,end) (assign_func pcc_name i)]
--     outer_loop_func pcc_name = [FieldLoop (start,end) (inner_loop_func pcc_name)]
--   in
--     return (pcc,outer_loop_func)


-- compileLOCToVectorFAST (inVar,outVar) (start,end) (ITensor n k lo) =
--   do
--     (pcc,inner_loop_func) <- compileLOToVectorFAST (inVar,outVar)
    