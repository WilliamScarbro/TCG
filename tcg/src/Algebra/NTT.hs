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
  deriving Eq
  
ffVec :: Integral a =>  a -> a -> (Int -> Int) -> Vector FF
ffVec n p f = Vec (matrix (fromIntegral n) 1 (\(x,y) -> Just (Res (toInteger (f (x-1))) (toInteger p))))

cannon_ffVec n p = ffVec n p id

linearOp :: Integral a => a -> ((Int,Int)->b) -> LinearOp b
linearOp n f = LO (matrix (fromIntegral n) (fromIntegral n) (\(x,y) -> f ((x-1),(y-1))))


-- haskell matrices are 1 indexed :\
get_el :: Int -> Int -> LinearOp a -> a
get_el x y (LO m) = getElem (x+1) (y+1) m

get_vec_el :: Int -> Int -> Vector a -> a
get_vec_el x y (Vec m) = getElem (x+1) (y+1) m

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

point_mult :: Vector FF -> Vector FF -> Maybe (Vector FF)
point_mult (Vec v1) (Vec v2) | nrows v1 /= nrows v2 = Nothing
                             | ncols v1 /= ncols v2 = Nothing
                             | nrows v1 /= 1 =
                               return (Vec (matrix (nrows v1) 1 (\(i,j) -> (getElem i 1 v1)*(getElem i 1 v2))))
                             | ncols v1 /= 1 =
                               return (Vec (matrix 1 (ncols v1) (\(i,j) -> (getElem 1 j v1)*(getElem 1 j v2))))
                                   
                                               
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
mT_perm di dj dk x =
  let
    i = div x (dj*dk)
    j = div (mod x (dj*dk)) dk
    k = mod x dk in
  (di*dk) * j + dk * i + k
   
  
mT :: Int -> Int -> Int -> LinearOp FF
mT di dj dk = perm (di*dj*dk) (mT_perm di dj dk)

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

phi_inv_func :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> FF
phi_inv_func n k d b p =
  let
    rd = mod d b
    m = div n k
    k_inv = inv (Res (toInteger k) (toInteger p))
  in
    (\x y ->
      do -- Maybe
        w <- nth_root b p :: Maybe ResInt
        w_inv <- inv w
        if mod x m == mod y m then
          --k_inv * (pow w_inv (div ((rd+(div x m)*b)*(div y m)) k))
          k_inv * (pow w_inv (div ((rd+(div y m)*b)*(div x m)) k))
        else
          0 )

phi_inv :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
phi_inv n k d b p =
  linearOp n (\(i,j) -> (phi_inv_func n k d b p i j)) -- reverses inputs (transposes) should fix to remove this

-- phi_inv :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
-- phi_inv n k d b p =
--   let rd = mod d b in
--     let w_inv=(nth_root b p) >>= inv in
--       let k_inv=inv (Res (toInteger k) (toInteger p)) in
--         let m=div n k in
--           --linearOp n (\(x,y) -> if mod x m == mod y m then Just (Res (toInteger (div ((rd+(div x m)*b)*(div y m)) k)) (toInteger p)) else 0)
--           linearOp n (\(x,y) -> if mod x m == mod y m then k_inv * (w_inv >>= (\z -> pow z (div ((rd+(div x m)*b)*(div y m)) k))) else 0)

-- gamma
-- extended by m to implement swapQQ
gamma_func :: Int -> Int -> Int -> Int -> Int -> Int -> FF
gamma_func k m d b p x = do
  omega <- nth_root b p
  pow omega ((div d k)*(div x m))

gamma :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
gamma k m d b p =
  let call = gamma_func k m d b p in
    linearOp (k*m) (\(x,y) -> if x==y then call x else 0)

gamma_inv_func :: Int -> Int -> Int -> Int -> Int -> Int -> FF
gamma_inv_func k m d b p x = do
  omega <- nth_root b p
  pow omega (b-(div d k)*(div x m))
  
gamma_inv :: Int -> Int -> Int -> Int -> Int -> LinearOp FF
gamma_inv k m d b p =
  let call = gamma_inv_func k m d b p in
    linearOp (k*m) (\(x,y) -> if x==y then call x else 0)


----
repeatLO :: Int -> LinearOp FF -> Maybe (LinearOp FF)
repeatLO k l = Just (tensor (mId k) l)

-- n == nrows (f i) \forall i
extendLO :: Int -> Int -> (Int -> Maybe (LinearOp FF)) -> Maybe (LinearOp FF)
extendLO n k f = Just (linearOp (n * k) (\(x,y) -> if div x n == div y n then (f (div x n)) >>= (\g -> get_el (mod x n) (mod y n) g) else 0))


apply_lo_list :: Vector FF -> [LinearOp FF] -> Maybe (Vector FF)
apply_lo_list vec (lo:lo_path) = do
  next <- mv lo vec
  apply_lo_list next lo_path
apply_lo_list vec [] = Just vec

combine_lo_list :: [LinearOp FF] -> Maybe (LinearOp FF)
combine_lo_list lo_list = foldr (\cur prev -> prev >>= (\p -> mm p cur)) (Just (head lo_list)) (tail lo_list)

instance Show (Int -> Maybe Kernel) where
  show f = show (f 0)
instance Eq (Int -> Maybe Kernel) where
  f1 == f2 = (f1 0) == (f2 0)
instance Ord (Int -> Maybe Kernel) where
  f1 <= f2 = (f1 0) <= (f2 0)

data Kernel
  = KInverse Kernel
  | Phi Int Int Int Int Int -- n k d b p
  | Gamma Int Int Int Int Int -- k m d b p
  | KL Int Int -- n k
  | KT Int Int Int -- di dj dk
  | KId Int -- n
  | Kernel_Extend Int Int (Int -> Maybe Kernel) -- n k f
  | Kernel_Repeat Int Int Kernel deriving (Show,Eq,Ord) -- n k


sizeof :: Kernel -> Int
sizeof (KInverse k) = sizeof k
sizeof (Phi n _ _ _ _) = n
sizeof (Gamma k m _ _ _) = k*m
sizeof (KT di dj dk) = di * dj * dk
sizeof (KL n _) = n
sizeof (KId n) = n
sizeof (Kernel_Extend n _ f) = n*(squashMaybeInt (f 0) sizeof)
sizeof (Kernel_Repeat n _ k) = n*(sizeof k)

-- there are other kernels which reduce to identity, but we ignore that here
isIdKer (KId _) = True
isIdKer _ = False

--define_kernel :: Kernel -> Maybe LinearOp FF
--define_kernel (Phi n k d b p) = phi 
