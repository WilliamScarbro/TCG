--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE OverlappingInstances #-}

module Algebra.FField where


import qualified Data.Set as Set

--import AbstractAlgebra.Fields

class Residue a where
  reduce :: a -> a
  comparable :: a -> a -> Bool
  
class ResidueRing a where
  add :: a -> a -> Maybe a
  neg :: a -> a
  sub :: a -> a -> Maybe a
  sub x y = add x (neg y) 
  mult :: a -> a -> Maybe a
  one :: a
  zero :: a

data ResInt = Res Integer Integer  

instance Residue ResInt where
  reduce (Res x 0) = Res x 0
  reduce (Res x p) = Res (mod x p) p
  comparable (Res x p) (Res y q) = p==q

instance Show ResInt where
  show (Res x p) = show x ++ "%" ++ show p

instance Eq ResInt where
  (==) = res_cmp (==)
  --x == y = get_rep x == get_rep y && comparable x y --get_mod x == get_mod y
  
instance Ord ResInt where
  (<=) = res_cmp (<=)
  --x <= y = get_rep x < get_rep y && comparable x y --get_mod x == get_mod y

instance ResidueRing ResInt where
  add = res_op (+)
  neg (Res x p) = Res (0-x) p
  mult = res_op (*)
  one = Res 1 0
  zero = Res 0 0

--- FF : public interface
type FF=Maybe ResInt

ff :: Integral a => a -> a -> FF
ff n p | is_prime p = Just (Res (toInteger n) (toInteger p))
       | otherwise = Nothing

instance Num FF where
  (+) = maybe_op add
  negate = fmap neg
  (-) = maybe_op sub
  (*) = maybe_op mult
  abs = id
  signum x = 1
  fromInteger x = Just (Res x 0)
----

maybe_op :: (a -> a -> Maybe a) -> (Maybe a -> Maybe a -> Maybe a)
maybe_op op (Just x) (Just y) = op x y
maybe_op op Nothing y = Nothing
maybe_op op x Nothing = Nothing

get_rep :: ResInt -> Integer
get_rep res = let Res y p = reduce res in y

get_mod :: ResInt -> Integer
get_mod ( Res x p ) = p

set_rep :: Integer -> Integer -> ResInt
set_rep p x = Res x p

res_op :: (Integer -> Integer -> Integer) -> (ResInt -> ResInt -> Maybe ResInt)
res_op op (Res x p) (Res y q) | (p /= q) && (p /= 0) && (q /= 0) = Nothing
                              | (p == 0) && (q /= 0) = res_op op (Res x q) (Res y q)
                              | (p /= 0) && (q == 0) = res_op op (Res x p) (Res y p)
                              | (p == q) = Just (reduce (Res (op x y) p))

res_cmp :: (Integer -> Integer -> Bool) -> (ResInt -> ResInt -> Bool)
res_cmp op (Res i p) (Res j q) | (p /= q) && (p /= 0) && (q /= 0) = False
                               | (p == 0) && (q /= 0) = res_cmp op (Res i q) (Res j q)
                               | (p /= 0) && (q == 0) = res_cmp op (Res i p) (Res j p)
                               | (p == q) = (op (mod i p) (mod j p))

is_prime :: (Integral a) => a -> Bool
is_prime x = length (divisors x) == 2
divisors :: (Integral a) => a -> [a]
divisors x = [1] ++ (filter (\z -> mod x z == 0) [2..floor . sqrt . fromIntegral $ x]) ++ [x]

power_set :: ResInt -> Set.Set ResInt
power_set (Res x p) = Set.map (set_rep p) (power_set_help x x p p)

power_set_help ::  (Integral a) => a -> a -> a -> a -> Set.Set a
power_set_help y z p 0  = Set.empty
power_set_help y z p q =  if q > 0 then Set.insert y ( power_set_help (mod (z*y) p) z p (q-1) ) else Set.empty
--
is_generator :: ResInt -> Bool
is_generator (Res x p) = ( toInteger . Set.size $ power_set (Res x p) ) == p-1
--
---- Utility functions
ff_generators :: (Integral a,Show a) => a -> [ResInt]
ff_generators p | is_prime p = let ip=toInteger p in map (set_rep ip) $ filter (\x -> is_generator (Res x ip) ) [1..ip-1]
                | otherwise = []
ff_generator :: (Integral a, Show a) => a -> ResInt
ff_generator p = let gens = ff_generators p in
  if gens==[] then error (show p++" is not prime") else head gens
--
ff_inv :: ResInt -> Maybe ResInt
ff_inv x | x==zero = Nothing
         | otherwise = let p=get_mod x in Just (fst ( head ( filter (\(y,z) -> z==Just one) [(y,x `mult` y) | y<-[Res i p| i<-[1..p-1]]] ) ) )

pow :: Integral a => ResInt -> a -> Maybe ResInt
pow (Res x p) e = pow_help (Res 1 p) (Res x p) e
pow_help :: Integral a => ResInt -> ResInt -> a -> Maybe ResInt
pow_help y x 0 = Just y
pow_help y x e | mod e 2 == 1 = do { sq <- mult x x
                                   ; res <- mult y x
                                   ; pow_help  res sq (div e 2) }
               | mod e 2 == 0 = do { sq <- mult x x
                                   ; pow_help y sq (div e 2) }

nth_root :: (Integral a,Show a) => a -> a -> FF
nth_root b p | mod (p-1) b /= 0 = Nothing
             | otherwise = pow (ff_generator p) (div (p-1) b)  

splitting_prime :: Integral a => a -> a
splitting_prime n = head (filter is_prime [n*i+1 | i<-[1..]])

non_triv_factors :: Integral a => a -> [a]
non_triv_factors n = filter (\x -> mod n x == 0 && x /= 1) ([2..(n `div` 2)]++[n])
class ResidueRing a => FiniteField a where
  inv :: a -> Maybe a
  divi :: a -> a -> Maybe a
  divi x y = ( inv y ) >>= mult x

instance FiniteField ResInt where
  inv = ff_inv
