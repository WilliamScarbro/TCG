module Compile.LOClasses where

import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger
import qualified Algebra.BetterAlgebra as BA

data LOClass
  = Diagonal Int (Int -> Int)
  | Permutation Int (Int -> Int)
  | Square Int (Int -> Int -> Int)
  | ITensor Int Int (LOClass) -- (LO \circledtimes I)
  | Partition Int [LOClass] -- m (size of each partition)
  | LOId Int

instance Show LOClass where
  show (Diagonal n f) = "Diagonal "++show n++":  "++(foldr (++) "" [show (f i)++"," |i<-[0..n-1]])
  show (Permutation n f) = "Permutation "++show n++" "++show [f i|i<-[0..n-1]]
  show (Square n f) = "Square "++show n
  show (ITensor n k lo) = "( "++show lo++" (ox) I_"++show k++")"
  show (Partition m arr) = "Partition "++show m++" "++show arr
  show (LOId n) = "LOId "++show n

-- not important, used to validate
loc_to_lo :: LOClass -> LinearOp Int
loc_to_lo (Diagonal n f) = linearOp n (\(i,j) -> if i==j then f i else 0)
loc_to_lo (Permutation n f) = linearOp n (\(i,j) -> if j == f i then 1 else 0)
loc_to_lo (Square n f) = linearOp n (uncurry f)
--loc_to_lo (ITensor n k loc) = 
  
isLOId (LOId _) = True
isLOId _ = False

isDiag (Diagonal _ _) = True
isDiag _ = False


kernelToLOC :: BA.ModPrimeMemo -> Kernel -> Maybe LOClass

kernelToLOC mpm (Phi n k d b p) =
  let
    square = (Square k (phi_func_memo mpm k k d)) 
  in
  if n==k then
    return square
  else
    return (ITensor n (div n k) square)
kernelToLOC mpm (KInverse (Phi n k d b p)) =
  let
    square = (Square k (phi_inv_func_memo mpm k k d))
  in
  if n==k then
    return square
  else
    return (ITensor n (div n k) square) 

kernelToLOC mpm (Gamma k m d b p) = return (Diagonal (k*m) (gamma_func_memo mpm k m d))
kernelToLOC mpm (KInverse (Gamma k m d b p)) = return (Diagonal (k*m) (gamma_inv_func_memo mpm k m d))

kernelToLOC _ (KT di dj dk) = return (Permutation (di*dj*dk) (mT_perm di dj dk)) -- applies inverse permutation (swaps di dj)
kernelToLOC _ (KInverse (KT di dj dk)) = return (Permutation (di*dj*dk) (mT_perm dj di dk)) -- applies inverse permutation (swaps di dj)

kernelToLOC _ (KL n k) = return (Permutation n (mL_perm n k))
kernelToLOC _ (KInverse (KL n k)) = return (Permutation n (mL_perm n (div n k)))

kernelToLOC _ (KId n) = return (LOId n)
kernelToLOC _ (KInverse (KId n)) = return (LOId n)

kernelToLOC mpm (Kernel_Extend n k f) =
    let kers = sequence (do{ -- List
      i<-[0..k-1]; -- Int
      ker <- return (f i); -- Maybe Kernel
      return (ker >>= (kernelToLOC mpm))}) in -- Maybe [LOCLass]
    kers >>= return . (Partition (div n k))
kernelToLOC mpm (KInverse (Kernel_Extend n k f)) =
    let kers = sequence (
          do -- List
            i<-[0..k-1] :: [Int]
            maybe_ker <- return (f i) :: [Maybe Kernel]
            return (do
                       ker <- maybe_ker :: Maybe Kernel
                       inv_ker <- return (KInverse ker) :: Maybe Kernel
                       lo <- (kernelToLOC mpm) inv_ker :: Maybe LOClass
                       return lo )) :: Maybe [LOClass]
    in
      kers >>= return . (Partition (div n k))


kernelToLOC mpm (Kernel_Repeat n k ker) = let kers = sequence [kernelToLOC mpm ker |i<-[0..k-1]] in
  kers >>= return . (Partition (div n k))
kernelToLOC mpm (KInverse (Kernel_Repeat n k ker)) =
  let
    inv_ker = KInverse ker
    lops = sequence [kernelToLOC mpm inv_ker |i<-[0..k-1]]
  in
    lops >>= return . (Partition (div n k))

