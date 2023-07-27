module Compile.KernelToFieldAST where

import Compile.LOClasses
import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger

getNextVar :: Int -> (Int, String)
getNextVar i = (i+1,"t"++show i)
 
_check_dims :: [String] -> [String] -> Int -> Bool
_check_dims fromVars toVars n = (length fromVars) == (length toVars) && (length toVars) == n

   
compileLOCToFieldAST :: ([String],[String]) -> LOClass  -> IO [FieldStmt a]
compileLOCToFieldAST (fromVars,toVars) (Diagonal n f) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Diagonal Compilation" (compileLOCHelp toVars n diagonal_expr)
  where
    diagonal_expr i = do { -- Maybe
      val <- f i >>= return . fromIntegral . get_rep;
      return (FieldOpExpr (FMultiply (Constant val) (Variable (fromVars !! i)))); }

compileLOCToFieldAST (fromVars,toVars) (Permutation n f) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Permutation Compilation" (compileLOCHelp toVars n (return . perm_expr))
  where
    perm_expr i = Variable (fromVars !! (f i))

compileLOCToFieldAST (fromVars,toVars) (Square n f) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Square Compilation" (compileLOCHelp toVars n square_expr)
  where
    square_expr :: Int -> Maybe (FieldExpr a)
    square_expr i = do { -- Maybe
      mult_exprs <- getExprs n i f fromVars; -- [FieldExpr a]
      return (foldr (\x y -> FieldOpExpr (FAdd x y)) (Constant 0 :: FieldExpr a) mult_exprs); }

    getExprs :: Int -> Int -> (Int -> Int -> FF) -> [String] -> Maybe [FieldExpr a]
    getExprs n i f vars = sequence ( [ f i j >>= return . fromIntegral . get_rep >>= (\val -> return (FieldOpExpr (FMultiply (Constant val) (Variable (fromVars !! j))))) | j <- [0..n-1]] )

compileLOCToFieldAST (fromVars,toVars) (Partition m lo_arr) =
  let
    n=m*(length lo_arr)
  in
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    (foldl foldKernelPar (return []) [(lo_arr!!i,(partitionArr fromVars n m)!!i,(partitionArr toVars n m)!!i) | i<-[0..(length lo_arr)-1]])
  where
    partitionArr arr n m = let k=div n m in
      [slice arr (m*i) (m*(i+1)) | i<-[0..k]]
    slice arr start end = take (end-start) . drop start $ arr

---- identity: (LO_m \ox I_k) = L^n_m (I_k \ox LO_m) L^n_k
compileLOCToFieldAST (fromVars,toVars) (ITensor n k lo) =
  compileLOCToFieldAST (permute_fromVars fromVars n k,permute_toVars toVars n k) (Partition (div n k) [lo | i<-[0..k-1]])
  where
    permute_fromVars inVars n k = [ inVars!!(mL_perm n (div n k) i) |i<-[0..n-1]]
    permute_toVars toVars n k = [ toVars!!(mL_perm n (div n k) i) | i<-[0..n-1]] -- inverse of inverse

compileLOCToFieldAST (fromVars,toVars) (LOId n) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Permutation Compilation" (compileLOCHelp toVars n (return . id_expr))
  where
    id_expr i = Variable (fromVars !! i)

  

--

compileLOCHelp :: [String] -> Int -> (Int -> Maybe (FieldExpr a)) -> Maybe [FieldStmt a]
compileLOCHelp toVars size f =
  let stmts =
        do
          i <- [0..size-1];
          expr <- return (f i);
          assign <- return (expr >>= (\ex -> Just (FieldAssignment (toVars !! i) ex)))
          return assign in
    sequence stmts


foldKernelPar ::  IO [FieldStmt a] -> (LOClass,[String],[String]) -> IO [FieldStmt a]
foldKernelPar io_stmts (lo,inVars,outVars) = do
    stmts <- io_stmts
    newStmts <- compileLOCToFieldAST (inVars,outVars) lo
    return (stmts++newStmts)


foldKernelSeries_inMem :: IO ([String],[String],[FieldStmt a]) -> LOClass ->  IO ([String],[String],[FieldStmt a])
foldKernelSeries_inMem io_varNames_stmts lo =
  do
    (inVars,outVars,stmts) <- io_varNames_stmts
    newStmts <- compileLOCToFieldAST (inVars,outVars) lo
    return (outVars,inVars,stmts++newStmts)

foldKernelSeries :: IO (Int,[String],[FieldStmt a]) -> LOClass -> IO (Int,[String],[FieldStmt a])
foldKernelSeries io_vc_inVars_stmts lo = do
  (vc,inVars,stmts) <- io_vc_inVars_stmts
  outVars <- return ["t"++show i |i<-[vc..vc+(length inVars)-1]]
  newStmts <- compileLOCToFieldAST (inVars,outVars) lo
  return (vc+(length outVars),outVars,stmts++newStmts)


                             
