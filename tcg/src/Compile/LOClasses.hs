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
  show (Diagonal n f) = "Diagonal "++show n
  show (Permutation n f) = "Permutation "++show n
  show (Square n f) = "Square "++show n
  show (ITensor n k lo) = "( "++show lo++" (ox) I_"++show k++")"
  show (Partition m arr) = "Partition "++show m++" "++show arr
  show (LOId n) = "LOId "++show n

isLOId (LOId _) = True
isLOId _ = False

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

