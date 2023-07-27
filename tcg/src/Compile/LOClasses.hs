module Compile.LOClasses where

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
  show (Diagonal n f) = "Diagonal "++show n++":  "++(foldr (++) "" [show (f i)++"," |i<-[0..n-1]])
  show (Permutation n f) = "Permutation "++show n++" "++show [f i|i<-[0..n-1]]
  show (Square n f) = "Square "++show n
  show (ITensor n k lo) = "( "++show lo++" (ox) I_"++show k++")"
  show (Partition m arr) = "Partition "++show m++" "++show arr
  show (LOId n) = "LOId "++show n

-- not important, used to validate
loc_to_lo :: LOClass -> LinearOp FF
loc_to_lo (Diagonal n f) = linearOp n (\(i,j) -> if i==j then f i else 0)
loc_to_lo (Permutation n f) = linearOp n (\(i,j) -> if j == f i then 1 else 0)
loc_to_lo (Square n f) = linearOp n (uncurry f)
--loc_to_lo (ITensor n k loc) = 
  
isLOId (LOId _) = True
isLOId _ = False

isDiag (Diagonal _ _) = True
isDiag _ = False


kernelToLOC :: Kernel -> Maybe LOClass

kernelToLOC (Phi n k d b p) =
  if n==k then
    return (Square n (phi_func n k d b p))
  else
    return (ITensor n (div n k) (Square k (phi_func k k d b p)))
kernelToLOC (KInverse (Phi n k d b p)) =
  if n==k then
    return (Square n (phi_inv_func n k d b p))
  else
    return (ITensor n (div n k) (Square k (phi_inv_func k k d b p)))

kernelToLOC (Gamma k m d b p) = return (Diagonal (k*m) (gamma_func k m d b p))
kernelToLOC (KInverse (Gamma k m d b p)) = return (Diagonal (k*m) (gamma_inv_func k m d b p))

kernelToLOC (KT di dj dk) = return (Permutation (di*dj*dk) (mT_perm dj di dk)) -- applies inverse permutation (swaps di dj)
kernelToLOC (KInverse (KT di dj dk)) = return (Permutation (di*dj*dk) (mT_perm di dj dk)) -- applies inverse permutation (swaps di dj)

kernelToLOC (KL n k) = return (Permutation n (mL_perm n k))
kernelToLOC (KInverse (KL n k)) = return (Permutation n (mL_perm n (div n k)))

kernelToLOC (KId n) = return (LOId n)
kernelToLOC (KInverse (KId n)) = return (LOId n)

kernelToLOC (Kernel_Extend n k f) =
    let kers = sequence (do{ -- List
      i<-[0..k-1]; -- Int
      ker <- return (f i); -- Maybe Kernel
      return (ker >>= kernelToLOC)}) in -- Maybe [LOCLass]
    kers >>= return . (Partition (div n k))
kernelToLOC (KInverse (Kernel_Extend n k f)) =
    let kers = sequence (
          do -- List
            i<-[0..k-1] :: [Int]
            maybe_ker <- return (f i) :: [Maybe Kernel]
            return (do
                       ker <- maybe_ker :: Maybe Kernel
                       inv_ker <- return (KInverse ker) :: Maybe Kernel
                       lo <- kernelToLOC inv_ker :: Maybe LOClass
                       return lo )) :: Maybe [LOClass]
    in
      kers >>= return . (Partition (div n k))


kernelToLOC (Kernel_Repeat n k ker) = let kers = sequence [kernelToLOC ker |i<-[0..k-1]] in
  kers >>= return . (Partition (div n k))
kernelToLOC (KInverse (Kernel_Repeat n k ker)) =
  let
    inv_ker = KInverse ker
    lops = sequence [kernelToLOC inv_ker |i<-[0..k-1]]
  in
    lops >>= return . (Partition (div n k))

