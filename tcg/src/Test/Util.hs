module Test.Util where

import Algebra.FField
import Algebra.NTT
import Algebra.Fourier
import Algebra.PolyRings
import Search.Search
import Util.Util

-- functions
baseOrder :: Ring -> Maybe [Int]
baseOrder (Prod n k f) =
  do --Maybe
    childs <- sequence [(f i) >>= baseOrder | i<-[0..k-1]]
    return (foldr (++) [] childs)
baseOrder (Quo n k d r) = baseOrder r
baseOrder (Base n d b p) = Just [d]


expandTerminal :: Ring -> Maybe [Int]
expandTerminal (Prod n k f) = fmap (foldr (++) []) (traverse id [(f i) >>= expandTerminal | i<- [0..k-1]])
expandTerminal (Base n d b p) = Just [d]
expandTerminal _ = Nothing

terminalToPerm :: Int -> Int -> Int -> [Int] -> [Int]
terminalToPerm n d b term = fmap (\x -> (x-(d `div` n)) `div` (b `div` n)) term

correctResult :: Ring -> Maybe [Int]
correctResult (Base n d b p) = let vec = ffVec n p id in
  let phi_lop = phi n n d b p in
    (mv phi_lop vec) >>= toIntList

applyPerm :: [Int] -> [Int] -> [Int]
applyPerm l perm = [if p < length l then l!!p else -1 | p <- perm]

testPerm :: Path -> Maybe ([Int],[Int])
testPerm path =
  let start = path_get_start path
      size = get_size start
      prime = get_prime start
      d = get_root_power start
      b = get_root start
      cannon = Path start [Factor size]  in
  do -- Maybe
    path_end <- path_get_end path
    path_term <- expandTerminal path_end
    path_perm <- return (terminalToPerm size d b path_term)
    cannon_end <- path_get_end cannon
    cannon_term <- expandTerminal cannon_end
    --cannon_perm <- return (terminalToPerm size d b cannon_term)
    perm_cannon_term <- return (applyPerm cannon_term path_perm )
    return (path_term,perm_cannon_term)

permCor :: Path -> Maybe [Int]
permCor path =
  let start = path_get_start path
      size = get_size start
      prime = get_prime start
      d = get_root_power start
      b = get_root start in
  do -- Maybe
    end <- path_get_end path
    cor <- correctResult start
    term <- expandTerminal end
    perm <- return (terminalToPerm size d b term)
    permCorList <- return (applyPerm cor perm)
    return permCorList
     
-- data

factor_path4 = Path (Base 4 0 4 5) [(Factor 2),(Extend 2 (Factor 2))]

factor_path8 = Path (Base 8 0 8 17) [(Factor 2),(Extend 2 (Factor 2)),(Extend 2 (Extend 2 (Factor 2)))]

spiral_path6 = Path (Base 6 0 6 7) [(Label 2), (Repeat 2 (Factor 3)), SwapQP, (Extend 3 (Define)), (Extend 3 (Norm)), (Extend 3 (Repeat 1 (Factor 2))), (Extend 3 SwapQP), (Extend 3 (Extend 2 Define))]

spiral_path6_nw = Path (Base 6 6 12 13) [(Label 2), (Repeat 2 (Factor 3)), SwapQP, (Extend 3 (Define)), (Extend 3 (Norm)), (Extend 3 (Repeat 1 (Factor 2))), (Extend 3 SwapQP), (Extend 3 (Extend 2 Define))]

spiral_path12 = Path (Base 12 0 12 37) [(Label 4), (Repeat 4 (Factor 3)), SwapQP, (Extend 3 (Define)), (Extend 3 (Norm)), (Extend 3 (Repeat 1 (Factor 4))), (Extend 3 SwapQP), (Extend 3 (Extend 4 Define))]

spiral_path12_nw = Path (Base 12 12 24 73) [(Label 4), (Repeat 4 (Factor 3)), SwapQP, (Extend 3 (Define)), (Extend 3 (Norm)), (Extend 3 (Repeat 1 (Factor 4))), (Extend 3 SwapQP), (Extend 3 (Extend 4 Define))]

spiral_path8 = Path (Base 8 0 8 17) [(Label 4), (Repeat 4 (Factor 2)), SwapQP, (Extend 2 (Define)), (Extend 2 (Norm)), (Extend 2 (Repeat 1 (Factor 4))), (Extend 2 SwapQP), (Extend 2 (Extend 4 Define))]

true_spiral_path6 = Path (Base 6 0 6 7) [(Label 2), (Repeat 2 (Factor 3)), SwapQP, (Extend 3 (Define)), (Extend 3 (Norm)), (Extend 3 (Repeat 1 (Factor 2))), (Extend 3 SwapQP), (Extend 3 (Extend 2 Define)), SwapJoinProd]

permuted_spiral_path6 = Path (Base 6 0 6 7) [(Label 2), (Repeat 2 (Factor 3)), SwapQP, (Extend 3 (Define)), (Extend 3 (Norm)), (Extend 3 (Repeat 1 (Factor 2))), (Extend 3 SwapQP), (Extend 3 (Extend 2 Define)), JoinProd]

join_path1 = Path (Base 16 0 16 17) [Factor 2,Extend 2 (Factor 4), JoinProd, Extend 8 (Factor 2)]
join_path2 = Path (Base 16 0 16 17) [Factor 2,Extend 2 (Factor 4), SwapJoinProd, Extend 8 (Factor 2)]
join_path3 = Path (Base 16 0 16 17) [Factor 2,Extend 2 (Factor 4), Extend 2 (Extend 4 (Factor 2)), SwapJoinProd,JoinProd]
join_path4 = Path (Base 16 0 16 17) [Factor 2,Extend 2 (Factor 4), Extend 2 (Extend 4 (Factor 2)), SwapJoinProd,SwapJoinProd]
join_path5 = Path (Base 16 0 16 17) [Factor 2,Extend 2 (Factor 4), Extend 2 (Extend 4 (Factor 2)), JoinProd,SwapJoinProd]


