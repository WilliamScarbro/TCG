--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Algebra.Fourier where

import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Util.Logger
import Util.Util

import Data.List
import Data.Maybe
import qualified Data.Map as Map (empty,insert,Map,member)
import System.Environment

--ENV Variables
matchContext = getEnv("MATCH_CONTEXT")


instance Show (Int -> Morphism) where
  show f = show (f 0)

instance Eq (Int -> Morphism) where
  (==) f1 f2 = (f1 0) == (f2 0)

data Morphism =
  MInverse Morphism |
  Extend Int Morphism |
  Repeat Int Morphism |
  Factor Int |
  Label Int |
  Norm |
  Define |
  SwapQQ | SwapQP | 
  JoinProd | SwapJoinProd |
  IdR
  deriving (Show,Eq,Ord)


is_leaf :: Morphism -> Bool
is_leaf (Extend _ _) = False
is_leaf (Repeat _ _) = False
is_leaf _ = True

split_parent_child :: Morphism -> Maybe (Morphism->Morphism,Morphism)
split_parent_child (Extend n morph) = Just ((\x -> Extend n x),morph)
split_parent_child (Repeat n morph) = Just ((\x -> Repeat n x),morph)
split_parent_child _ = Nothing

apply :: Morphism -> Ring -> Maybe Ring
apply (Label k) x = (label k) x 
apply (Factor k) x = (factor k) x
apply (Norm) x = norm x
apply Define x = define x 
apply SwapQQ x = swapQQ x
apply SwapQP x = swapQP x
--apply SwapPQ x = swapPQ x
apply JoinProd x = joinProd x 
apply SwapJoinProd x = swapPP x >>= joinProd
apply (Extend k0 m) (Prod n k f) | k0==k = Just (Prod n k (\i -> f i >>= (\g -> apply m g)))  --- ( int -> Maybe R ) >>= (Ring -> Maybe Ring)  :: Int -> Maybe Ring
                                 | otherwise = Nothing
apply (Repeat i m) (Quo n j k x) | i==j = (apply m x) >>= (\y -> Just (Quo n j k y))
                                 | otherwise = Nothing
apply IdR x = Just x
apply (MInverse m) _ = Nothing -- cannot apply Inverse Morphism
apply _ _ = Nothing

io_apply :: Morphism -> Ring -> IO Ring
io_apply m r = maybeToIO ("Failed apply '"++show m++"' '"++show r++"'") $ apply m r

---

data Match = Match (Ring -> IO [Morphism])

match :: Match -> Ring -> IO [Morphism]
match (Match m) r = m r

matchAdd :: Match -> Match -> Match
matchAdd (Match m1) (Match m2) = Match (\r -> do { m1r <- (m1 r);
                                                   m2r <- m2 r;
                                                   return (m1r ++ m2r); } )

infixl 2 <+>
(<+>)=matchAdd

functorMatch = (Match matchExtend) <+> (Match matchRepeat)
factorMatch = functorMatch <+> (Match matchFactor)
permuteMatch =
  factorMatch <+>
  (Match matchLabel) <+>
  (Match matchDefine) <+>
  (Match matchSwap)
normalizeMatch = permuteMatch <+> (Match matchNorm)
joinMatch = normalizeMatch <+> (Match matchJoinProd) -- add others later
morphismMatch :: IO Match
morphismMatch = do { -- IO
  mc <- matchContext;
  let correctMatch = (filter (\x -> fst x==mc ) [("Factor",factorMatch),("Permute",permuteMatch),("Normalize",normalizeMatch),("Join",joinMatch)]) in
      if length correctMatch == 0 then (logObj "MATCH_CONTEXT value invalid "  mc) >> (return factorMatch) else return (snd (head (correctMatch))); }

morphismMatchExtend = do
  mc <- matchContext
  mm <- morphismMatch
  if mc=="Normalize" || mc=="Join" then return (mm <+> (Match matchNormExtend)) else return mm
  
  --matchMorphism :: Match
--matchMorphism = (Match matchExtend) <+> (Match matchRepeat) <+> (Match matchFactor) <+>  -- <+> (Match matchId) <+> (Match matchNorm)
--matchMorphism = (Match matchExtend) <+> (Match matchRepeat) <+> (Match matchFactor)
-- 

-- insures that any matched morphism can be applied to entire domain of f 
matchExtend :: Ring -> IO [Morphism]
matchExtend (Prod n k f) = 
  --let morphs=[(maybeToList (f i)) >>= (\r -> match matchMorphism r) | i<-[0..k-1]] -- [[Morph]]
  --in if morphs==[] then [] else fmap (\x -> Extend k x) (foldr intersect (head morphs) morphs)
  do { -- IO
    mm <- morphismMatchExtend; -- Match
    morphs <- sequence (do { -- []
        i <- [0..k-1]; -- Int
        ring <- maybeToList (f i); -- Ring
        io_morphs <- return (match mm ring); -- IO [Morph]
        return io_morphs; }); -- [[Morph]]
    return (if morphs==[] then [] else fmap (\x -> Extend k x) (foldr intersect (head morphs) morphs)) }
matchExtend r = return []

matchRepeat :: Ring -> IO [Morphism]
matchRepeat (Quo n k d r) = do { mm <- morphismMatch; -- Match
                                 morphs <- match mm r; -- [Morphs]
                                 return (fmap (\x -> Repeat k x) morphs) }
matchRepeat r = return []

matchFactor :: Ring -> IO [Morphism]
matchFactor (Base n d b p) = return [Factor k | k <- (non_triv_factors n) ]
matchFactor r = return []

matchLabel :: Ring -> IO [Morphism]
matchLabel (Base n d b p) = return [Label k | k <- (filter (\x -> x /= n) (non_triv_factors n)) ]
matchLabel r = return []

matchNorm :: Ring -> IO [Morphism]
matchNorm (Base n d b p) | d /= 0 && n /= 1 = return [Norm]
                         | otherwise = return []
matchNorm r = return []

matchNormExtend :: Ring -> IO [Morphism]
matchNormExtend (Base n d b p) | n /= 1 = return [Norm]
                               | otherwise = return []
matchNormExtend r = return []

matchDefine :: Ring -> IO [Morphism]
matchDefine (Quo n k d0 (Base 1 d b p)) = return [Define]
matchDefine r = return []

matchSwap :: Ring -> IO [Morphism]
matchSwap (Quo n0 k 0 (Quo n1 1 d r)) = return [SwapQQ] -- QQ
matchSwap (Quo n0 k0 d0 (Prod n1 k1 f)) = return [SwapQP] -- QP
matchSwap _ = return []

matchJoinProd (Prod n0 k0 f) -- | isQuo (f 0) = return [SwapPQ] -- PQ
                         | isProd (f 0) = return [JoinProd,SwapJoinProd] -- PP
                         | otherwise = return []
  where
    isProd (Just (Prod _ _ _)) = True 
    isProd _ = False
    isQuo (Just (Quo _ _ _ _)) = True
    isQuo _ = False
matchJoinProd _ = return [] 

matchId :: Ring -> IO [Morphism]
matchId r = return [IdR]

---


---

define_morphism :: Morphism -> Ring -> Maybe (LinearOp FF)

define_morphism (Factor k) (Base n d b p) = Just (phi n k d b p)
define_morphism (Factor k) _ = Nothing
define_morphism (MInverse (Factor k)) (Base n d b p) = Just (phi_inv n k d b p)
define_morphism (MInverse (Factor k)) _ = Nothing

define_morphism (Label k) (Base n d b p) = Just (mL n k)
define_morphism (Label k) r = Nothing
define_morphism (MInverse (Label k)) (Base n d b p) = Just (mL n (div n k))
define_morphism (MInverse (Label k)) _ = Nothing

define_morphism Norm (Base n d b p) = Just (gamma n 1 d b p)
define_morphism Norm r = Nothing
define_morphism (MInverse Norm) (Base n d b p) = Just (gamma_inv n 1 d b p)
define_morphism (MInverse Norm) _ = Nothing

define_morphism Define (Quo n k d0 (Base 1 d b p)) = Just (mId n)
define_morphism Define r = Nothing
define_morphism (MInverse Define) (Quo n k d0 (Base 1 d b p)) = Just (mId n)
define_morphism (MInverse Define) r = Nothing

define_morphism SwapQQ (Quo n0 k 0 (Quo n1 1 d r)) = Just (gamma k n1 d (get_root r) (get_prime r))
define_morphism SwapQQ _ = Nothing
define_morphism (MInverse SwapQQ) (Quo n0 k 0 (Quo n1 1 d r)) = Just (gamma_inv k n1 d (get_root r) (get_prime r))
define_morphism (MInverse SwapQQ) _ = Nothing

define_morphism SwapQP (Quo n0 k0 d0 (Prod n1 k1 f)) = Just (mT k0 k1 (div n1 k1))
define_morphism SwapQP _ = Nothing
define_morphism (MInverse SwapQP) (Quo n0 k0 d0 (Prod n1 k1 f)) = Just (mT k1 k0 (div n1 k1))
define_morphism (MInverse SwapQP) _ = Nothing

--define_morphism SwapPQ (Prod n0 k0 f) =
--  do
--    (n1,k1) <- get_quo_vals (f 0)
--    return (mT n0 k0 (div n1 k1))
--  where
--    get_quo_vals Just (Quo n1 k1 _ _) = Just (n1,k1)
--    get_quo_vals _ = Nothing
--define_morphism SwapPQ _ = Nothing
define_morphism SwapJoinProd (Prod n0 k0 f0) =
  do
    prod <- f0 0
    (n1,k1,f1) <- prod_get_data prod
    return (mT k0 k1 (div n1 k1))
define_morphism SwapJoinProd _ = Nothing
define_morphism (MInverse SwapJoinProd) (Prod n0 k0 f0) =
  do
    prod <- f0 0
    (n1,k1,f1) <- prod_get_data prod
    return (mT k1 k0 (div n1 k1))
define_morphism (MInverse SwapJoinProd) _ = Nothing

define_morphism JoinProd (Prod n _ _) = Just (mId n)
define_morphism JoinProd _ = Nothing
define_morphism (MInverse JoinProd) (Prod n _ _) = Just (mId n)
define_morphism (MInverse JoinProd) _ = Nothing

define_morphism (Repeat k0 m) (Quo n k1 d r) = if k0 == k1 then (define_morphism m r) >>= (\lo -> (repeatLO k0 lo)) else Nothing
define_morphism (Repeat k0 m) r = Nothing
define_morphism (MInverse (Repeat k0 m)) (Quo n k1 d r) = if k0 == k1 then (define_morphism (MInverse m) r) >>= (\lo -> (repeatLO k0 lo)) else Nothing
define_morphism (MInverse (Repeat k0 m)) r = Nothing

define_morphism (Extend k0 m) (Prod n k1 f) = if k0 == k1 then extendLO (div n k1) k1 (\i -> (f i) >>= (\r -> define_morphism m r)) else Nothing
define_morphism (Extend k0 m) r = Nothing
define_morphism (MInverse (Extend k0 m)) (Prod n k1 f) = if k0 == k1 then extendLO (div n k1) k1 (\i -> (f i) >>= (\r -> define_morphism (MInverse m) r)) else Nothing
define_morphism (MInverse (Extend k0 m)) r = Nothing


--


morphism_to_kernel :: Morphism -> Ring -> Maybe Kernel

morphism_to_kernel (Factor k) (Base n d b p) = Just (Phi n k d b p)
morphism_to_kernel (Factor k) r = Nothing

morphism_to_kernel (Label k) (Base n d b p) = Just (KL n (div n k))
morphism_to_kernel (Label k) r = Nothing

morphism_to_kernel Norm (Base n d b p) = Just (Gamma n 1 d b p)
morphism_to_kernel Norm r = Nothing

morphism_to_kernel Define (Quo n k d0 (Base 1 d b p)) = Just (KId n)
morphism_to_kernel Define r = Nothing

morphism_to_kernel SwapQQ (Quo n0 k 0 (Quo n1 1 d r)) = Just (Gamma k n1 d (get_root r) (get_prime r))
morphism_to_kernel SwapQQ _ = Nothing

morphism_to_kernel SwapQP (Quo n0 k0 d0 (Prod n1 k1 f)) = Just (KT k0 k1 (div n1 k1))
--morphism_to_kernel SwapPQ (Prod n0 k0 f) =
--  do
--    ring <- f 0
--    (n1,k1,_,_) <- quo_get_vals ring
--    return (KT n0 k0 (div n1 k1))
--morphism_to_kernel SwapPQ _ = Nothing
--morphism_to_kernel SwapPP (Prod n0 k0 f) =
--  do
--    ring <- f 0
--    (n1,k1,_) <- prod_get_vals ring
--    return (KT n0 k0 (div n1 k1))
--morphism_to_kernel SwapPP _ = Nothing
morphism_to_kernel JoinProd (Prod n _ _) = Just (KId n)
morphism_to_kernel JoinProd _ = Nothing
                            
morphism_to_kernel SwapJoinProd (Prod n0 k0 f0) = 
  do
    prod <- f0 0
    (n1,k1,f1) <- prod_get_data prod
    return (KT k0 k1 (div n1 k1))
morphism_to_kernel SwapJoinProd _ = Nothing

morphism_to_kernel (Repeat k0 m) (Quo n k1 d r) = if k0 == k1 then (morphism_to_kernel m r) >>= (\lo -> Just (Kernel_Repeat n k0 lo)) else Nothing
morphism_to_kernel (Repeat k0 m) r = Nothing

morphism_to_kernel (Extend k0 m) (Prod n k f) = Just (Kernel_Extend n k0 (\i -> (f i) >>= (\r -> morphism_to_kernel m r)))
morphism_to_kernel (Extend k0 m) r = Nothing 


morphism_to_kernel (MInverse m) ring = (morphism_to_kernel m ring) >>= (\k -> return (KInverse k))

--
                            
is_par_morph :: Morphism -> Morphism -> Bool
is_par_morph base (Repeat k m) = base == (Repeat k m) || is_par_morph base m
is_par_morph base (Extend k m) = base == (Extend k m) || is_par_morph base m 
is_par_morph base m = base == m

morph_get_inner :: Morphism -> Morphism
morph_get_inner (Repeat k m) = morph_get_inner m
morph_get_inner (Extend k m) = morph_get_inner m
morph_get_inner m = m

patternMatchMorphism :: (Morphism -> Bool) -> Morphism -> Bool
patternMatchMorphism f (Repeat k m) = if f (Repeat k m) then True else patternMatchMorphism f m
patternMatchMorphism f (Extend k m) = if f (Extend k m) then True else patternMatchMorphism f m
patternMatchMorphism f m = f m
