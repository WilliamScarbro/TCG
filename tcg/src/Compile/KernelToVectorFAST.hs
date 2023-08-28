{-# LANGUAGE ScopedTypeVariables #-}

module Compile.KernelToVectorFAST where

import Compile.LOClasses
import Compile.CAST
import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger
import Compile.Monty
import qualified Algebra.BetterAlgebra as BA

import Data.List (nub,elemIndex)
import Control.Monad

--compileLOCToFieldAST :: ([String],[String]) -> LOClass  -> IO [FieldStmt a]

-- pre computed constants
data PCC a = PCC {pcc_size::Int, vals::[a]} deriving (Eq,Show)

pcc_concat (PCC size1 vals1) (PCC size2 vals2) = PCC (size1+size2) (vals1++vals2)

-- this should be part of FieldAST
arr_index :: String -> String -> String
arr_index arr index = arr++"["++index++"]"

data VectorClass a =
  DiagonalVector Int Int [PCC a] |
  SquareVector Int Int [PCC a] |
  ITensorVector Int Int Int [PCC a] |
  PermutationVector Int Int PermClass
  deriving Show

data PermClass =
  LPerm Int Int |
  TPerm Int Int Int |
  IdPerm Int
  deriving (Eq,Show)

kernelToVectorClass :: BA.ModPrimeMemo -> Kernel -> Maybe (VectorClass Int)
kernelToVectorClass mpm (Phi n k d b p) =
  let pcc = [PCC (k*k) (join [[phi_func_memo mpm k k d i j | j<- [0..k-1]] | i <- [0..k-1]])] in
  if n==k then
    return (SquareVector 1 k pcc)
    else
    return (ITensorVector 1 k (div n k) pcc)
    
kernelToVectorClass mpm (Gamma k m d b p) = return (DiagonalVector 1 (k*m) [PCC (k*m) [gamma_func_memo mpm k m d i |i<-[0..(k*m)-1]]])

kernelToVectorClass _ (KT di dj dk) = let n = di*dj*dk in
  return (PermutationVector 1 n (TPerm di dj dk))

kernelToVectorClass _ (KL n k) =
  return (PermutationVector 1 n (LPerm n k))

        
kernelToVectorClass mpm (Kernel_Extend n k f) =
  do
    vcs <- sequence [f i >>= kernelToVectorClass mpm | i<-[0..k-1]]
    foldl combineVectorClasses (return . head $ vcs) (tail vcs)

kernelToVectorClass mpm (Kernel_Repeat n k ker) =
  do
    vc <- kernelToVectorClass mpm ker
    let vcs = [vc | i<-[0..k-1]]
    foldl combineVectorClasses (return . head $ vcs) (tail vcs)
    
combineVectorClasses :: Maybe (VectorClass a) -> (VectorClass a) -> Maybe (VectorClass a)
combineVectorClasses (Just (DiagonalVector 1 k1 pcc1))  (DiagonalVector 1 k2 pcc2) = return (DiagonalVector 1 (k1+k2) [pcc_concat (head pcc1) (head pcc2)])
                                                                                    --  otherwise = Nothing
-- combineVectorClasses (Just (DiagonalVector n1 k1 pcc1))  (DiagonalVector n2 k2 pcc2) | k1 == k2 = return (DiagonalVector (n1+n2) k1 (pcc1++pcc2))
--                                                                                     | otherwise = Nothing
combineVectorClasses (Just (SquareVector n1 k1 pcc1))  (SquareVector n2 k2 pcc2) | k1 == k2 = return (SquareVector (n1+n2) k1 (pcc1++pcc2))
                                                                                | otherwise = Nothing
combineVectorClasses (Just (ITensorVector n1 k1 m1 pcc1)) (ITensorVector n2 k2 m2 pcc2) | k1 == k2 && m1 == m2 = return (ITensorVector (n1+n2) k1 m1 (pcc1++pcc2))
                                                                                  | otherwise = Nothing
combineVectorClasses (Just (PermutationVector n1 k1 pclass1)) (PermutationVector n2 k2 pclass2) | k1 == k2 && pclass1 == pclass2 = return (PermutationVector (n1+n2) k1 pclass1)
                                                                                                | otherwise = Nothing
combineVectorClasses _ _ = Nothing


--
 
vectorClassToC :: Field a => a -> String -> String -> Int -> VectorClass b -> [CStatement]
vectorClassToC field inVar outVar pcc_index vc =
  case vc of
      (DiagonalVector n k pccs) ->
        return $ CLoop (0,n) (\i -> return $ diagonal_func field [litExpr k, inExpr i k , outExpr i k, from_pcc_lib pcc_index i])
      (SquareVector  n k pccs) ->
        return $ CLoop (0,n) (\i -> return $ square_func field [litExpr k, inExpr i k , outExpr i k, from_pcc_lib pcc_index i])
      (ITensorVector n k m pccs) ->
        return $ CLoop (0,n) (\i -> return $ itensor_func field [litExpr k, litExpr m, inExpr i (m*k), outExpr i (m*k), from_pcc_lib pcc_index i])
      (PermutationVector n k pclass) ->
          case pclass of
            TPerm di dj dk -> return $ CLoop (0,n) (\i -> return $ CFuncStmt "TPerm" [litExpr di,litExpr dj,litExpr dk,inExpr i k, outExpr i k])
            LPerm n2 k2 -> return $ CLoop (0,n) (\i -> return $ CFuncStmt "LPerm" [litExpr n2,litExpr k2, inExpr i k, outExpr i k])
            IdPerm n2 -> return $ CFuncStmt "Id" [litExpr (n*n2),Identifier inVar,Identifier outVar]
  where
    inExpr i k = CBinaryOp CAdd (Identifier inVar) (CBinaryOp CMultiply (Literal (IntLiteral k)) (Identifier i))
    outExpr i k =  CBinaryOp CAdd (Identifier outVar) (CBinaryOp CMultiply (Literal (IntLiteral k)) (Identifier i))
    litExpr n = Literal (IntLiteral n)

    from_pcc_lib :: Int -> String -> CExpr
    from_pcc_lib pcc_index i = Identifier ("pccMap["++show pcc_index++"]["++i++"]")
                  

associateVCWithPCCList :: Eq a => VectorClass a -> [PCC a] -> Maybe [Int]
associateVCWithPCCList vc pccList =
  case vc of
    (DiagonalVector n k pccs) -> matchPCCList pccs pccList
    (SquareVector n k pccs) -> matchPCCList pccs pccList
    (ITensorVector n k m pccs) -> matchPCCList pccs pccList
    _ -> return []
 where
   matchPCCList :: Eq a => [PCC a] -> [PCC a] -> Maybe [Int]
   matchPCCList pccs pccList =
     sequence . (fmap (\pcc -> elemIndex pcc pccList)) $ pccs
   
buildPCCList :: Eq a => [VectorClass a] -> [PCC a]
buildPCCList vcs = nub $ buildPCCList_help vcs
  where
    buildPCCList_help ((DiagonalVector n k pccs):vcs) = nub (pccs++buildPCCList_help vcs)
    buildPCCList_help ((SquareVector n k pccs):vcs)  = nub (pccs++buildPCCList_help vcs)
    buildPCCList_help ((ITensorVector n k m pccs):vcs) = nub (pccs++buildPCCList_help vcs)
    buildPCCList_help ((PermutationVector n k pclass):vcs) = nub (buildPCCList_help vcs)
    buildPCCList_help [] = []  
  
containsPCCs :: VectorClass a -> Bool
containsPCCs (DiagonalVector _ _ _) = True
containsPCCs (SquareVector _ _ _ ) = True
containsPCCs (ITensorVector _ _ _ _) = True
containsPCCs _ = False

-- returns pcc_init, gen_func, pcc_dest
-- CAST cannot represent pointers so we produce code lines ([String]) for init and dest
compileVectorClasses :: Field a => a -> Int -> [VectorClass Int] -> Maybe ([String],[CStatement],[String])
compileVectorClasses field size vclasses =
  let
    vclasses_odd = if mod (length vclasses) 2 == 0 then vclasses++[PermutationVector 1 size (IdPerm size)] else vclasses
    pccList = buildPCCList vclasses_odd :: [PCC Int]
  in
    do -- Maybe
      pccMap <- sequence $ fmap (\vc -> associateVCWithPCCList vc pccList) vclasses_odd :: Maybe [[Int]]
      let vectorCAST = join $ do -- []
            index <- [0..length vclasses_odd-1]
            let vc = vclasses_odd !! index
            let (inVar,outVar) = if mod index 2 == 0 then ("X","Y") else ("Y","X")
            return $ vectorClassToC field inVar outVar index vc
      let createListCode = createPCCListCode field pccList
      let createMapCode = createPCCMapCode pccMap
      let destroyCode = destroyPCCCode pccList pccMap
      return (createListCode++createMapCode,vectorCAST,destroyCode)
      
--

malloc name ptype size = name++"=("++ptype++"*)malloc(sizeof("++ptype++")*"++show size++");"

createPCCListCode :: Field a => a -> [PCC Int] -> [String]
createPCCListCode field pccList =
  let
    list_init = malloc "*pccList_pointer" "int*" (length pccList)
    list_assign = "int** pccList=*pccList_pointer;"
    element_init = foldr (++) [] $ do
        i <- [0..length pccList-1]
        let pcc = pccList !! i
        let ff_values = vals  pcc
        let str_vals = fmap show . fmap (to_int field) $ ff_values --  fmap (to_int field) . fmap fromIntegral . fmap get_rep $
        let static = "int pcc"++show i++"["++(show . pcc_size $ pcc)++"]={"++showStrTuple str_vals++"};"
        let dynamic = "int *dypcc"++show i++"=allocate("++showStrTuple [show $ pcc_size pcc,"pcc"++show i]++");"
        let assign = "pccList["++show i++"]=dypcc"++show i++";"
        return [static,dynamic,assign] :: [[String]]
  in
    (list_init:(list_assign:element_init))

    -- int** pccMap1 = (int**)malloc(2*sizeof(int*));
    -- pccMap1[0]=pccList[0];
    -- pccMap1[1]=pccList[1];

createPCCMapCode :: [[Int]] -> [String]
createPCCMapCode pccMap =
  let
    map_init = malloc "*pccMap_pointer" "int**" (length pccMap)
    map_assign = "int*** pccMap=*pccMap_pointer;"
    element_init = foldr (++) [] $ do
      i <- [0..length pccMap-1]
      let map = pccMap !! i
      let allocate = "int** "++malloc ("pccMap"++show i) "int*" (length map) :: String
      let el_assign = ["pccMap"++show i++"["++show j++"]=pccList["++show (map!!j)++"];" | j<-[0..length map-1]] :: [String]
      let assign = "pccMap["++show i++"]=pccMap"++show i++";" :: String
      return ([allocate,assign]++el_assign) :: [[String]]
  in
    map_init:(map_assign:element_init)

destroyPCCCode :: [PCC a] -> [[Int]] -> [String]
destroyPCCCode pccList pccMap =
  let
    list_el_free = for_free "pccList" (length pccList)
    map_el_free = for_free "pccMap" (length pccMap)
    list_free = "free(pccList);"
    map_free = "free(pccMap);"
  in
    list_el_free++map_el_free++[list_free,map_free]
  where
    for_free name size =
      ["for (int i=0; i<"++show size++"; i++){",
       "  free("++name++"[i]);",
       "}",
       ""]
    
-- vectorBoilerPlate :: String -> CStatement -> String -> Int -> String
-- vectorBoilerPlate pccInit code pccDestroy size =
  
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
    
