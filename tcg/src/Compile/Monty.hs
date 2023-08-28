module Compile.Monty where

import Algebra.BetterAlgebra

data Monty = Monty { p :: Int, r :: Int, pPrime :: Int, rInv :: Int, monty_mpm :: ModPrimeMemo}

instance Show Monty where
  show (Monty p r pPrime rInv _) = "Monty P:" ++ show p ++ " R:" ++ show r ++ " P':" ++ show pPrime ++ " R_inv:" ++ show rInv


monty_init :: Int -> Int -> Monty
monty_init p b =
  let
    r = (2^((truncate ((log (fromIntegral p)) / (log 2))) + 1))
    mpm = init_ModPrimeMemo p b 
    rInv = inverse mpm r
    pPrime = r*rInv `div` p
  in
    Monty p r pPrime rInv mpm

  -- where
  --   findInverse :: Int -> Int -> Maybe Int
  --   findInverse r p =
  --     let
  --       inverseList = filter (\i -> (r * i) `mod` p == 1) [0..p-1]
  --     in
  --       case inverseList of
  --         [rInv] -> Just rInv
  --         _ -> Nothing

redc :: Monty -> Int -> Int
redc monty t = let
  m = ((t `mod` r monty) * pPrime monty) `mod` r monty
  t' = (t + m * p monty) `div` r monty
  res = if t' < p monty then t' else t' - p monty
  in
  if res == (t * (rInv monty)) `mod` p monty then
    res
  else
    error "Assertion failed: res == (t * fromJust (rInv monty)) `mod` n monty"

multiplyResidue :: Monty -> Int -> Int -> Int
multiplyResidue monty i j = redc monty (i * j)

addResidue :: Monty -> Int -> Int -> Int
addResidue _ i j = i + j

subResidue :: Monty -> Int -> Int -> Int
subResidue _ i j = i - j

int2Residue :: Monty -> Int -> Int
int2Residue monty x = redc monty ((x `mod` p monty) * (r monty * r monty `mod` p monty))

residue2Int :: Monty -> Int -> Int
residue2Int monty x = redc monty x

--data MontyResidue = MontyResidue { monty :: Monty, residue :: Int }
--
--instance Show MontyResidue where
--  show (MontyResidue monty residue) = "MontyResidue: res: " ++ show residue ++ " val: " ++ show (getVal monty residue)
--
--getVal :: Monty -> Int -> Int
--getVal monty res = residue2Int monty res
--
--multiplyMontyResidue :: MontyResidue -> MontyResidue -> Maybe MontyResidue
--multiplyMontyResidue lhs rhs = let
--  monty = monty lhs
--  res = multiplyResidue monty (residue lhs) (residue rhs)
--  in
--  if monty == monty rhs then
--    Just (MontyResidue monty res)
--  else
--    Nothing
--
--addMontyResidue :: MontyResidue -> MontyResidue -> Maybe MontyResidue
--addMontyResidue lhs rhs = let
--  monty = monty lhs
--  res = addResidue monty (residue lhs) (residue rhs)
--  in
--  if monty == monty rhs then
--    Just (MontyResidue monty res)
--  else
--    Nothing
--
--subMontyResidue :: MontyResidue -> MontyResidue -> Maybe MontyResidue
--subMontyResidue lhs rhs = let
--  monty = monty lhs
--  res = subResidue monty (residue lhs) (residue rhs)
--  in
--  if monty == monty rhs then
--    Just (MontyResidue monty res)
--  else
--    Nothing
