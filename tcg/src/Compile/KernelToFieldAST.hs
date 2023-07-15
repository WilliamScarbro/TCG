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

   
compileLOToFieldAST :: ([String],[String]) -> LOClass  -> IO [FieldStmt a]
compileLOToFieldAST (fromVars,toVars) (Diagonal n f) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Diagonal Compilation" (compileLOHelp toVars n diagonal_expr)
  where
    diagonal_expr i = do { -- Maybe
      val <- f i >>= return . fromIntegral . get_rep;
      return (FieldOpExpr (FMultiply (Constant val) (Variable (fromVars !! i)))); }

compileLOToFieldAST (fromVars,toVars) (Permutation n f) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Permutation Compilation" (compileLOHelp toVars n (return . perm_expr))
  where
    perm_expr i = Variable (fromVars !! (f i))

compileLOToFieldAST (fromVars,toVars) (Square n f) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Square Compilation" (compileLOHelp toVars n square_expr)
  where
    square_expr :: Int -> Maybe (FieldExpr a)
    square_expr i = do { -- Maybe
      mult_exprs <- getExprs n i f fromVars; -- [FieldExpr a]
      return (foldr (\x y -> FieldOpExpr (FAdd x y)) (Constant 0 :: FieldExpr a) mult_exprs); }

    getExprs :: Int -> Int -> (Int -> Int -> FF) -> [String] -> Maybe [FieldExpr a]
    getExprs n i f vars = sequence ( [ f i j >>= return . fromIntegral . get_rep >>= (\val -> return (FieldOpExpr (FMultiply (Constant val) (Variable (fromVars !! j))))) | j <- [0..n-1]] )

compileLOToFieldAST (fromVars,toVars) (Partition m lo_arr) =
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
compileLOToFieldAST (fromVars,toVars) (ITensor n k lo) =
  compileLOToFieldAST (permute_fromVars fromVars n k,permute_toVars toVars n k) (Partition (div n k) [lo | i<-[0..k-1]])
  where
    permute_fromVars inVars n k = [ inVars!!(mL_perm n (div n k) i) |i<-[0..n-1]]
    permute_toVars toVars n k = [ toVars!!(mL_perm n (div n k) i) | i<-[0..n-1]] -- inverse of inverse

compileLOToFieldAST (fromVars,toVars) (LOId n) =
  if not (_check_dims fromVars toVars n) then
    logObj "LO2FAST: check dims failed" n >> return []
  else
    maybeToIO "failed Permutation Compilation" (compileLOHelp toVars n (return . id_expr))
  where
    id_expr i = Variable (fromVars !! i)

  

--

compileLOHelp :: [String] -> Int -> (Int -> Maybe (FieldExpr a)) -> Maybe [FieldStmt a]
compileLOHelp toVars size f =
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
    newStmts <- compileLOToFieldAST (inVars,outVars) lo
    return (stmts++newStmts)


foldKernelSeries_inMem :: IO ([String],[String],[FieldStmt a]) -> LOClass ->  IO ([String],[String],[FieldStmt a])
foldKernelSeries_inMem io_varNames_stmts lo =
  do
    (inVars,outVars,stmts) <- io_varNames_stmts
    newStmts <- compileLOToFieldAST (inVars,outVars) lo
    return (outVars,inVars,stmts++newStmts)

foldKernelSeries :: IO (Int,[String],[FieldStmt a]) -> LOClass -> IO (Int,[String],[FieldStmt a])
foldKernelSeries io_vc_inVars_stmts lo = do
  (vc,inVars,stmts) <- io_vc_inVars_stmts
  outVars <- return ["t"++show i |i<-[vc..vc+(length inVars)-1]]
  newStmts <- compileLOToFieldAST (inVars,outVars) lo
  return (vc+(length outVars),outVars,stmts++newStmts)



--
---- parameterize with Field, eventually
---- vCount, linearOp, varList 
--compileLOToFieldAST :: Int -> LOClass -> [String] -> IO (Int,[String],[FieldStmt a])
--compileLOToFieldAST vc (Diagonal n f) vars =
--  if length vars /= n then
--    logObj "LO2FAST: num vars does not match diagonal dimensions" (length vars,n) >> return (0,[],[])
--  else
--    maybeToIO "failed Diagonal Compilation" (compileLOHelp vc n vars diagonal_expr)
--  where
--    diagonal_expr i = do { -- Maybe
--      val <- f i >>= return . fromIntegral . get_rep;
--      return (FieldOpExpr (FMultiply (Constant val) (Variable (vars !! i)))); }
--
--compileLOToFieldAST fromVar toVar start (Permutation n f) =
--  maybeToIO "failed Perm Compilation" (compileLOHelp_inMem toVar start size perm_expr)
--  where
--    perm_expr i = Variable (fromVar++"["++(start+(f i))++"]")
--    
--compileLOToFieldAST vc (Permutation n f) vars = let new_vars = [ vars !! (f i) | i <-[0..n-1]] in
--  maybeToIO "failed Perm Compilation" (Just (vc,new_vars,[]))
--
--compileLOToFieldAST_inMem fromVar toVar start (Square n f) =
--  maybeToIO "failed Square Compilation" (compileLOHelp_inMem toVar start size square_expr)
--  where
--    square_expr :: Int -> Maybe (FieldExpr a)
--    square_expr i = do
--      mult_expr <- getExprs fromVar n i f
--
--    getExprs :: String -> Int -> Int -> (Int -> Int -> FF) -> [String] -> Maybe [FieldExpr a]
--    getExprs fromVar n i f = sequence ( 
--compileLOToFieldAST vc (Square n f) vars =
--  if length vars /= n then
--    logObj "LO2FAST: num vars does not match square dimensions" (length vars,n) >> return (0,[],[])
--  else
--    maybeToIO "failed Square Compilation" (compileLOHelp vc n vars square_expr)
--  where
--    square_expr :: Int -> Maybe (FieldExpr a)
--    square_expr i = do { -- Maybe
--      mult_exprs <- getExprs n i f vars; -- Maybe [FieldExpr a]
--      return (foldr (\x y -> FieldOpExpr (FAdd x y)) (Constant 0 :: FieldExpr a) mult_exprs); }
--
--    getExprs :: Int -> Int -> (Int -> Int -> FF) -> [String] -> Maybe [FieldExpr a]
--    getExprs n i f vars = sequence ( [ f i j >>= return . fromIntegral . get_rep >>= (\val -> return (FieldOpExpr (FMultiply (Constant val) (Variable (vars !! j))))) | j <- [0..n-1]] )
--
--compileLOToFieldAST vc (Partition m lo_arr) vars =
--  if length vars /= m*(length lo_arr) then
--    logObj "LO2FAST: num vars does not match partition dimensions" (length vars,m,length lo_arr) >> return (0,[],[])
--  else
--    (foldl foldKernelPar (return (vc,[],[])) [(lo_arr!!i,[vars!!j|j<-[i*m..(i+1)*m-1]])|i<-[0..(length lo_arr)-1]])
--
--compileLOToFieldAST vc (LOId n) vars =
--  if length vars /= n then
--    logObj "LO2FAST: num vars does not match Id dimensions" (length vars,n) >> return (0,[],[])
--  else
--    return (vc,vars,[])
--
---- identity: (LO_m \ox I_k) = L^n_m (I_k \ox LO_m) L^n_k
--compileLOToFieldAST vc (ITensor n k lo) vars = do
--  kers <- maybeToIO "LO2FAST: ITensor failed kernel construction" $ sequence [kernelToLO (KL n (div n k)),Just (Partition (div n k) [lo | i<-[0..k-1]]),kernelToLO (KL n k)]
--  foldl foldKernelSeries (return (vc,vars,[])) kers
--
---- helper functions
--
--foldKernelPar ::  IO (Int,[String],[FieldStmt a]) -> (LOClass,[String]) -> IO (Int,[String],[FieldStmt a])
--foldKernelPar io_vc_outVars_stmts (lo,inVars) = do
--    (vc,outVars,stmts) <- io_vc_outVars_stmts
--    (newVC,newVars,newStmts) <- compileLOToFieldAST vc lo inVars
--    return (newVC,outVars++newVars,stmts++newStmts)

  
                                         
--compileLOHelp :: Int -> Int -> [String] -> (Int -> Maybe (FieldExpr a)) -> Maybe (Int,[String],[FieldStmt a])
--compileLOHelp vc n vars f =
--  let temp = sequence (do { -- List
--                          i <- [0..n-1];
--                            newVar <- return ("t"++show (i+vc)); -- String
--                            expr <- return (f i); -- Maybe (FieldExpr a)
--                            assign <- return (expr >>= (\ex -> Just (FieldAssignment newVar ex))); -- Maybe (FieldStmt a)
--                            return (assign >>= (\x -> Just (newVar,x)))}) in
--    do{ -- Maybe
--      uw_temp <- temp;
--      lastVC <- return (vc+n);
--      vars <- return (fmap fst uw_temp);
--      stmts <- return (fmap snd uw_temp);
--      return (lastVC,vars,stmts); }
                             
