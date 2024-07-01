module Algebra.Algorithms where

import Algebra.PolyRings


register_tiles :: Int -> Int -> (Int -> Morphism) -> Morphism
register_tiles n k inner =
  let
    divnk = div n k
  in
    (Stride k) <.>
    (OExpFunc k (inner (divnk))) <.>
    SwapOP <.>
    ProdFunc divnk (
      DefineO <.>
      inner k) <.>
    JoinProd

in_place :: Morphism
in_place =
  Norm <.>
  RotFunc (Factor 2) <.>
  SwapRP <.>
  ProdFunc 2 DefineR

seiler :: Int -> Morphism
seiler 2 = in_place
seiler n =
  Clump (div n 2) <.>
  IExpFunc (div n 2) in_place <.>
  SwapIP <.>
  ProdFunc 2 (
    DefineI <.>
    seiler (div n 2)) <.>
  JoinProd
   
seiler_register_tiles :: Morphism
seiler_register_tiles = register_tiles 256 4 seiler

------------------------------

spiral_4_step :: Int -> Int -> Morphism
spiral_4_step n k =
  let m = div n k in
    Factor k <.>
    ProdFunc k (
      Norm <.>
      RotFunc (Factor m) <.>
      SwapRP <.>
      ProdFunc m DefineR) <.>
    SwapJoinProd

-----------------
-- coppersmith QFT

-- Base(2^k,d) -> I(2)^(k-1) B(2,d)
qubitify :: Int -> Morphism
qubitify l | l<=1 = IdR
             | l==2 = Clump 2
             | otherwise = Clump 2 <.>
                           IExpFunc 2 (qubitify (l-1))

-- unwraps l many IExpFunc
qubitFunctor :: Int -> (Morphism -> Morphism)
qubitFunctor l m
  | l < 0 = error "qubitFunctor called with l < 0"
  | l==0 = m 
  | otherwise = IExpFunc 2 (qubitFunctor (l-1) m)

qqft l = qubitify l <.> qft l
-- assumes input is of form I(2)^(l-1) B(2,0) (qubitified) (for l>1)
-- produces output of form P(2) R(i*N/2^(l-1)) I(2)^(l-2) B(2,0) 
qft :: Int -> Morphism
qft l | l==1 = Factor 2
      | otherwise =
        qubitFunctor (l-2) (
          IExpFunc 2 (Factor 2) <.>
          SwapIP <.>
          ProdFunc 2 (
              DefineI <.>
              Norm )) <.>
        (foldr (<.>)  IdR [qubitFunctor (l-i) SwapIP | i<-[3..l]]) <.>
        ProdFunc 2 (
          (foldr (<.>)  IdR [qubitFunctor (l-i) SwapIR | i<-[3..l]] ) <.>
          RotFunc (qft (l-1)) <.>
          SwapRP <.>
          ProdFunc (2^(l-1)) DefineR ) <.>
        JoinProd

--qft_cleanup l | l==1 
        
-- Doesn't work: normalizing bases with different rotations is not 1 gate operation (DefineR cannot be used)
-- assumes input is of form I(2)^(l-1) B(2,0) (qubitified)
-- produces output of form P(2) I(2)^(l-2) R B(2,0) 
qft_better :: Int -> Morphism
qft_better l = qft_better_head l qft_better_tail

-- I(2)^(l-1) B(2,0) -> P(2) I(2)^(l-2) R B(2,0)
qft_better_head l tail | l==1 = Factor 2
                       | otherwise =
   qubitFunctor (l-2) (
     IExpFunc 2 (Factor 2) <.>
     SwapIP <.>
     ProdFunc 2 (
       DefineI <.>
       Norm )) <.>
     (foldr (<.>)  IdR [qubitFunctor (l-i) SwapIP | i<-[3..l]]) <.>
     ProdFunc 2 (tail (l-1))
        
-- I(2)^(l-1) R B(2,0) -> P(2) I(2)^(l-2) R B(2,0)
qft_better_tail l | l==1 = RotFunc (Factor 2) <.> SwapRP
                      | otherwise = 
      qubitFunctor (l-2) (
        IExpFunc 2 ( -- (IExp 2)*(l-2)( IExp 2 (Rot (Base 2)))
          RotFunc (Factor 2) <.> -- (IExp 2)*(l-2)( IExp 2 (Rot (Prod 2 (Base 1))))
          SwapRP <.> -- (IExp 2)*(l-2)( IExp 2 (Prod 2 (Rot (Base 1))))
          ProdFunc 2 DefineR) <.>  -- (IExp 2)*(l-2)( IExp 2 (Prod 2 (Base 1)))
        SwapIP <.>  -- (IExp 2)*(l-2)( Prod 2 (IExp 2 (Base 1)))
        ProdFunc 2 (
          -- SwapIR <.> -- (IExp 2)*(l-2)( Prod 2 (IExp 2 (Base 1)))
          DefineI <.> -- (IExp 2)*(l-2)( Prod 2 (Base 2))
          Norm)) <.>  -- (IExp 2)*(l-2)( Prod 2 (Rot (Base 2)))
      (foldr (<.>)  IdR [qubitFunctor (l-i) SwapIP | i<-[3..l]]) <.>
        -- Prod 2 ((IExp 2)*(l-2)( Rot (Base 2)))
      ProdFunc 2 (qft_better_tail (l-1))

-- I(2)^(l-1) R B(2,0) -> P(2) I(2)^(l-2) R B(2,0)
qft_better_2 :: Int -> Morphism
qft_better_2 l = qft_better_head l qft_better_2_tail
  where
    qft_better_2_tail l | l==1 = RotFunc (Factor 2) <.> SwapRP
                        | otherwise =
      qubitFunctor (l-2) (
        IExpFunc 2 (
          RotFunc (Factor 2) <.>
          SwapRP ) <.>
        SwapIP <.> -- (IExp 2)*(l-2)( Prod 2 (IExp 2 (Rot (Base 1))))
        ProdFunc 2 (
          SwapIR <.> -- (IExp 2)*(l-2)( Prod 2 (Rot (IExp 2 (Base 1))))
          RotFunc (
            DefineI <.> -- (IExp 2)*(l-2)( Prod 2 (Rot (Base 2)))
            Norm) <.>  -- (IExp 2)*(l-2)( Prod 2 (Rot (Rot (Base 2))))
          JoinRot)) <.> -- (IExp 2)*(l-2)( Prod 2 (Rot (Base 2)))
      (foldr (<.>)  IdR [qubitFunctor (l-i) SwapIP | i<-[3..l]]) <.>
      ProdFunc 2 (qft_better_2_tail (l-1))
        
qft8 :: Morphism
qft8 =
  Clump 2 <.> -- IExp 2 (Base 4)
  IExpFunc 2 (
    Clump 2 <.>  -- IExp 2 (IExp 2 (Base 2))
    IExpFunc 2 (
      Factor 2) <.> -- IExp 2 (IExp 2 (Prod 2 (Base 1)))
    SwapIP <.>  -- IExp 2 (Prod 2 (IExp 2 (Base 1)))
    ProdFunc 2 (
      DefineI <.> -- IExp 2 (Prod 2 (Base 2))
      Norm )) <.>  -- IExp 2 (Prod 2 (Rot (Base 2)))
  SwapIP <.> -- Prod 2 (IExp 2 (Rot (Base 2)))
  ProdFunc 2 (
    SwapIR <.> -- Prod 2 (Rot (IExp 2 (Base 2)))
    RotFunc (
      IExpFunc 2 (Factor 2) <.>  -- Prod 2 (Rot (IExp 2 (Prod 2 (Base 1))))
      SwapIP <.> -- Prod 2 (Rot (Prod 2 (IExp 2 (Base 1))))
      ProdFunc 2 (
        DefineI <.> -- Prod 2 (Rot (Prod 2 (Base 2)))
        Norm) <.>  -- Prod 2 (Rot (Prod 2 (Rot (Base 2))))
      ProdFunc 2 (RotFunc (Factor 2)))) -- Prod 2 (Rot (Prod 2 (Rot (Prod 2 (Base 1)))))
      
               
bitreversal :: Int -> Morphism
bitreversal l | l==1 = Factor 2
              | otherwise =
                Factor 2 <.>
                ProdFunc 2 (bitreversal (l-1)) <.>
                JoinProd
