module Compile.KernelToFieldAST where

import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger

data LOClass
  = Diagonal Int (Int -> FF)
  | Permutation Int (Int -> Int)
  | Square Int (Int -> Int -> FF)
  | ITensor Int Int (LOClass) -- (LO \circledtimes I)
  | Partition Int [LOClass] -- m (size of each partition)
  | LOId Int

instance Show LOClass where
  show (Diagonal n f) = "Diagonal "++show n
  show (Permutation n f) = "Permutation "++show n
  show (Square n f) = "Square "++show n
  show (ITensor n k lo) = "( "++show lo++" (ox) I_"++show k++")"
  show (Partition m arr) = "Partition "++show m++" "++show arr
  show (LOId n) = "LOId "++show n
   
kernelToLO :: Kernel -> Maybe LOClass
kernelToLO (Phi n k d b p) =
  if n==k then
    return (Square n (phi_func n k d b p))
  else
    return (ITensor n (div n k) (Square k (phi_func k k d b p)))
kernelToLO (Gamma k m d b p) = return (Diagonal (k*m) (gamma_func k m d b p))
kernelToLO (KT di dj dk) = return (Permutation (di*dj*dk) (mT_perm dj di dk)) -- applies inverse permutation (swaps di dj)
kernelToLO (KL n k) = return (Permutation n (mL_perm n k))
kernelToLO (KId n) = return (LOId n)
kernelToLO (Kernel_Extend n k f) =
    let kers = sequence (do{ -- List
      i<-[0..k-1]; -- Int
      ker <- return (f i); -- Maybe Kernel
      return (ker >>= kernelToLO)}) in -- Maybe [LOCLass]
    kers >>= return . (Partition (div n k))
kernelToLO (Kernel_Repeat n k ker) = let kers = sequence [kernelToLO ker |i<-[0..k-1]] in
  kers >>= return . (Partition (div n k))


getNextVar :: Int -> (Int, String)
getNextVar i = (i+1,"t"++show i)
 

type CodeBlock = (Int,[String],[FieldStmt FF])
cb_getVC (vc,_,_) = vc
cb_getVars (_,vars,_) = vars
cb_getStmts (_,_,stmts) = stmts

-- parameterize with Field, eventually
-- vCount, linearOp, varList 
compileLOToFieldAST :: Int -> LOClass -> [String] -> IO (Int,[String],[FieldStmt a])
compileLOToFieldAST vc (Diagonal n f) vars =
  if length vars /= n then
    logObj "LO2FAST: num vars does not match diagonal dimensions" (length vars,n) >> return (0,[],[])
  else
    maybeToIO "failed Diagonal Compilation" (compileLOHelp vc n vars diagonal_expr)
  where
    diagonal_expr i = do { -- Maybe
      val <- f i >>= return . fromIntegral . get_rep;
      return (FieldOpExpr (FMultiply (Constant val) (Variable (vars !! i)))); }
      
compileLOToFieldAST vc (Permutation n f) vars = let new_vars = [ vars !! (f i) | i <-[0..n-1]] in
  maybeToIO "failed Perm Compilation" (Just (vc,new_vars,[]))
  
compileLOToFieldAST vc (Square n f) vars =
  if length vars /= n then
    logObj "LO2FAST: num vars does not match square dimensions" (length vars,n) >> return (0,[],[])
  else
    maybeToIO "failed Square Compilation" (compileLOHelp vc n vars square_expr)
  where
    square_expr :: Int -> Maybe (FieldExpr a)
    square_expr i = do { -- Maybe
      mult_exprs <- getExprs n i f vars; -- Maybe [FieldExpr a]
      return (foldr (\x y -> FieldOpExpr (FAdd x y)) (Constant 0 :: FieldExpr a) mult_exprs); }

    getExprs :: Int -> Int -> (Int -> Int -> FF) -> [String] -> Maybe [FieldExpr a]
    getExprs n i f vars = sequence ( [ f i j >>= return . fromIntegral . get_rep >>= (\val -> return (FieldOpExpr (FMultiply (Constant val) (Variable (vars !! j))))) | j <- [0..n-1]] )

compileLOToFieldAST vc (Partition m lo_arr) vars =
  if length vars /= m*(length lo_arr) then
    logObj "LO2FAST: num vars does not match partition dimensions" (length vars,m,length lo_arr) >> return (0,[],[])
  else
    (foldl foldKernelPar (return (vc,[],[])) [(lo_arr!!i,[vars!!j|j<-[i*m..(i+1)*m-1]])|i<-[0..(length lo_arr)-1]])

compileLOToFieldAST vc (LOId n) vars =
  if length vars /= n then
    logObj "LO2FAST: num vars does not match Id dimensions" (length vars,n) >> return (0,[],[])
  else
    return (vc,vars,[])

-- identity: (LO_m \ox I_k) = L^n_m (I_k \ox LO_m) L^n_k
compileLOToFieldAST vc (ITensor n k lo) vars = do
  kers <- maybeToIO "LO2FAST: ITensor failed kernel construction" $ sequence [kernelToLO (KL n (div n k)),Just (Partition (div n k) [lo | i<-[0..k-1]]),kernelToLO (KL n k)]
  foldl foldKernelSeries (return (vc,vars,[])) kers

-- helper functions

foldKernelPar ::  IO (Int,[String],[FieldStmt a]) -> (LOClass,[String]) -> IO (Int,[String],[FieldStmt a])
foldKernelPar io_vc_outVars_stmts (lo,inVars) = do
  (vc,outVars,stmts) <- io_vc_outVars_stmts
  (newVC,newVars,newStmts) <- compileLOToFieldAST vc lo inVars
  return (newVC,outVars++newVars,stmts++newStmts)

foldKernelSeries :: IO (Int,[String],[FieldStmt a]) -> LOClass -> IO (Int,[String],[FieldStmt a])
foldKernelSeries io_vc_inVars_stmts lo = do
  (vc,inVars,stmts) <- io_vc_inVars_stmts
  (newVC,outVars,newStmts) <- compileLOToFieldAST vc lo inVars
  return (newVC,outVars,stmts++newStmts)
  

compileLOHelp :: Int -> Int -> [String] -> (Int -> Maybe (FieldExpr a)) -> Maybe (Int,[String],[FieldStmt a])
compileLOHelp vc n vars f =
  let temp = sequence (do { -- List
                          i <- [0..n-1];
                            newVar <- return ("t"++show (i+vc)); -- String
                            expr <- return (f i); -- Maybe (FieldExpr a)
                            assign <- return (expr >>= (\ex -> Just (FieldAssignment newVar ex))); -- Maybe (FieldStmt a)
                            return (assign >>= (\x -> Just (newVar,x)))}) in
    do{ -- Maybe
      uw_temp <- temp;
        lastVC <- return (vc+n);
        vars <- return (fmap fst uw_temp);
        stmts <- return (fmap snd uw_temp);
        return (lastVC,vars,stmts); }
                             
