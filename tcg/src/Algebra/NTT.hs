{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Algebra.NTT where

import Algebra.FField
import Algebra.PolyRings

import Data.Matrix


data LinearOp a where
  LO :: (Matrix a) -> LinearOp a
data Vector a where
  Vec :: (Matrix a) -> Vector a

ffVec :: Integral a =>  a -> a -> (Int -> Int) -> Vector FF
ffVec n p f = Vec (matrix (fromIntegral n) 1 (\(x,y) -> Just (Res (toInteger (f (x-1))) (toInteger p))))

linearOp :: Integral a => a -> ((Int,Int)->b) -> LinearOp b
linearOp n f = LO (matrix (fromIntegral n) (fromIntegral n) (\(x,y) -> f ((x-1),(y-1))))


-- haskell matrices are 1 indexed :\
get_el :: Int -> Int -> LinearOp a -> a
get_el x y (LO m) = getElem (x+1) (y+1) m

size :: LinearOp a -> Int
size (LO m) = nrows m

----

toIntList :: Vector FF -> Maybe [Int]
toIntList (Vec m) = let fflist = if ncols m > nrows m then [getElem 1 i m | i<-[1..ncols m]] else [getElem i 1 m | i<-[1..nrows m]] in
  fmap (fmap (fromIntegral . get_rep)) (sequenceA fflist)

lo_op :: (Matrix a -> Matrix a -> Matrix a) -> (LinearOp a -> LinearOp a -> LinearOp a)
lo_op op (LO x) (LO y) = LO (op x y)

lo_cmp :: (Matrix a -> Matrix a -> Bool) -> (LinearOp a -> LinearOp a -> Bool)
lo_cmp op (LO x) (LO y) = op x y


instance Show (LinearOp FF) where
  show (LO m) = show m

instance Show (Vector FF) where
  show (Vec v) = show v

instance Functor LinearOp where
  -- (a->b) -> (f a->f b) = 
  fmap f (LO m) = LO (fmap f m)

instance Num (LinearOp FF) where
  (+) = lo_op (+)
  negate = fmap negate
  (-) = lo_op (-)
  (*) = lo_op (*)
  abs = id
  signum x = 1
  fromInteger x = linearOp 1 (\(z,y) -> fromInteger x :: FF)

instance Eq (LinearOp FF) where
  (==) = lo_cmp (==)

---

mm :: LinearOp FF -> LinearOp FF -> Maybe (LinearOp FF)
mm (LO m) (LO m2) | ncols m /= nrows m2 = Nothing
                  | otherwise = Just (LO (m*m2))

mv :: LinearOp FF -> Vector FF -> Maybe (Vector FF)
mv (LO m) (Vec v) | ncols m /= nrows v = Nothing
                  | otherwise = Just (Vec (m*v))

vm :: Vector FF -> LinearOp FF -> Maybe (Vector FF)
vm (Vec v) (LO m) | ncols v /= nrows m = Nothing
                  | otherwise = Just (Vec (v*m))

vv :: Vector FF -> Vector FF -> Maybe (Vector FF)
vv (Vec v) (Vec v2) | ncols v /= nrows v2 = Nothing
                    | otherwise = Just (Vec (v*v2))

tensor :: LinearOp FF -> LinearOp FF -> LinearOp FF
tensor l1 l2 = let n1=size l1 in let n2=size l2 in
  linearOp (n1 * n2) (\(x,y) ->  (get_el (div x n2) (div y n2) l1) * (get_el (mod x n2) (mod y n2) l2))

tpose :: LinearOp FF -> LinearOp FF
tpose l = linearOp (size l) (\(x,y) -> get_el y x l)

--tpose :: Vector FF -> Vector FF
--tpose v = Vector 
---------


mId :: Integral a => a -> LinearOp FF
mId n = linearOp n (\(i,j) -> if i==j then Just one else Just Algebra.FField.zero)

-- assumes (Int -> Int) is a bijection
perm :: Int -> (Int -> Int) -> LinearOp FF
perm n f = linearOp n (\(i,j) -> if f (j)==i then Just one else Just Algebra.FField.zero)

mT_perm :: Int -> Int -> Int -> Int -> Int
mT_perm n k block x = let m = div n k in
  let xBlock = div x block in
  (div xBlock k) + m * (mod xBlock x) + (mod x block)

mT :: Int -> Int -> Int -> LinearOp FF
mT n k block = perm n (mT_perm n k block)

mL_perm :: Int -> Int -> Int -> Int
mL_perm n k x = let m = fromIntegral (div n k) in let ik = fromIntegral k in (div x ik) + m * (mod x ik)

mL :: Int -> Int -> LinearOp FF
mL n k = perm n (mL_perm n k)
--mL n k = let m = fromIntegral (div n k) in let ik = fromIntegral k in perm n (\x -> (div x ik) + m * (mod x ik))

---

mNTT :: (Integral a,Show a) =>  a -> a -> LinearOp FF
mNTT n p = let w=nth_root n p in (linearOp n (\(i,j) -> w >>= (\x -> pow x (i*j) )))

mNTT_inv :: (Integral a,Show a) => a -> a -> LinearOp FF
mNTT_inv n p =
  let w_inv=(nth_root n p) >>= inv in
    let n_inv=inv (Res (toInteger n) (toInteger p)) in
      (linearOp n (\(i,j) -> n_inv * (w_inv >>= (\x -> pow x (i*j)))))

phi_func :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> FF
phi_func n k d b p =
  let rd = mod d b 
      w = nth_root b p 
      m = div n k in
        (\x y -> if mod x m == mod y m then w >>= (\z -> pow z (div ((rd+(div x m)*b)*(div y m)) k)) else 0)

--phi n k d b p
phi :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
-- x,y -> let z=x//m , j=x%m, i=y//m, j'=y%m in if j==j' then w_b^(rd+z*b)*i/k
phi n k d b p = let call=(\(i,j) -> phi_func n k d b p i j) in
  linearOp n call

phi_inv :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
phi_inv n k d b p =
  let rd = mod d b in
    let w_inv=(nth_root b p) >>= inv in
      let k_inv=inv (Res (toInteger k) (toInteger p)) in
        let m=div n k in
          --linearOp n (\(x,y) -> if mod x m == mod y m then Just (Res (toInteger (div ((rd+(div x m)*b)*(div y m)) k)) (toInteger p)) else 0)
          linearOp n (\(x,y) -> if mod x m == mod y m then k_inv * (w_inv >>= (\z -> pow z (div ((rd+(div x m)*b)*(div y m)) k))) else 0)

gamma_func :: Int -> Int -> Int -> Int -> Int -> FF
gamma_func n d b p x = 
  let w=nth_root b p >>= (\z -> pow z (div (b-d) n)) in
      w >>= (\z -> pow z x)

gamma :: Int -> Int -> Int -> Int -> LinearOp FF
gamma n d b p =
  let call = gamma_func n d b p in
    linearOp n (\(x,y) -> if x==y then call x else 0)

gamma_inv :: Int -> Int -> Int -> Int -> LinearOp FF
gamma_inv n d b p =
  let w=nth_root b p >>= (\z -> pow z (div (b-d) n)) in
    linearOp n (\(x,y) -> if x==y then w >>= (\z -> pow z (b-x)) else 0)


----
repeatLO :: Int -> LinearOp FF -> Maybe (LinearOp FF)
repeatLO k l = Just (tensor (mId k) l)

-- n == nrows (f i) \forall i
extendLO :: Int -> Int -> (Int -> Maybe (LinearOp FF)) -> Maybe (LinearOp FF)
extendLO n k f = Just (linearOp (n * k) (\(x,y) -> if div x n == div y n then (f (div x n)) >>= (\g -> get_el (mod x n) (mod y n) g) else 0))


instance Show (Int -> Maybe Kernel) where
  show f = show (f 0)
instance Eq (Int -> Maybe Kernel) where
  f1 == f2 = (f1 0) == (f2 0)
instance Ord (Int -> Maybe Kernel) where
  f1 <= f2 = (f1 0) <= (f2 0)

data Kernel
  = Phi Int Int Int Int Int -- n k d b p
  | Gamma Int Int Int Int -- n d b p
  | KL Int Int -- n k
  | KT Int Int Int -- n k m
  | KId Int -- n
  | Kernel_Extend Int Int (Int -> Maybe Kernel) -- n k f
  | Kernel_Repeat Int Int Kernel deriving (Show,Eq,Ord) -- n k


sizeof :: Kernel -> Int
sizeof (Phi n _ _ _ _) = n
sizeof (Gamma n _ _ _) = n
sizeof (KT n _ _) = n
sizeof (KL n _) = n
sizeof (KId n) = n
sizeof (Kernel_Extend n _ f) = n*(squashMaybeInt (f 0) sizeof)
sizeof (Kernel_Repeat n _ k) = n*(sizeof k)


--define_kernel :: Kernel -> Maybe LinearOp FF
--define_kernel (Phi n k d b p) = phi 
