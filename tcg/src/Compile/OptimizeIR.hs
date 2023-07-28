module Compile.OptimizeIR where

import Compile.FAST
import Algebra.NTT
import Algebra.FField
import Util.Util
import Util.Logger
import Search.Search
import Search.HillClimbing
import Compile.LOClasses
import Algebra.Fourier

replace_swapjoinprod :: Path -> Path
replace_swapjoinprod (Path start morphs) = Path start (fmap rsjp_helper morphs)
  where
    rsjp_helper SwapJoinProd = JoinProd
    rsjp_helper (Repeat n m) = (Repeat n (rsjp_helper m))
    rsjp_helper (Extend n m) = (Extend n (rsjp_helper m))
    rsjp_helper x = x
    
remove_identity_kernels :: [Kernel] -> [Kernel]
remove_identity_kernels kers = filter (not . isIdKer) kers

-- expand diagonal
expand_diagonal :: LOClass -> LOClass
expand_diagonal (Partition m los) =
  let
    all_diag = all id (fmap isDiag los)
    maybe_expanded = (do --Maybe
      diag_func_list <- sequence (fmap get_diag_func los) :: Maybe [Int -> FF]
      diag_func <-return (\i -> (diag_func_list !! (div i m)) (mod i m)) :: Maybe (Int -> FF)
      if all_diag then
        Just (Diagonal (m*(length los)) diag_func)
      else
        Nothing)
  in
    case maybe_expanded of
      Just x -> x
      Nothing -> (Partition m los)
  where
    get_diag_func (Diagonal n f) = Just f
    get_diag_func _ = Nothing
expand_diagonal x = x

--type ExpandFunc a = a -> Path -> IO (a,[Path])

replace_swapjoinprod_in_expandfunc :: ExpandFunc a -> ExpandFunc a
replace_swapjoinprod_in_expandfunc expand_func dummy path =
  do
    (new_dummy,paths) <- expand_func dummy path
    modified_paths <- return (fmap replace_swapjoinprod paths)
    return (new_dummy,modified_paths)
