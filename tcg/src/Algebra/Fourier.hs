--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Algebra.Fourier where

import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Util.Logger

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

data Morphism = Inverse Morphism | Extend Int Morphism | Repeat Int Morphism | Factor Int | Label Int | Norm | Define | Pushin | IdR deriving (Show,Eq)


--instance Show Morphism where
--  show Compose m1 m2 = show m1++" . "++shw m2
--  show Extend k f = "Extend "++show k++" "++show (f 0)
--  show Repeat k m = "Repeat "++show k++" "++show m
--  show Factor k = "Factor "++show k++

apply :: Morphism -> Ring -> Maybe Ring
apply (Label k) x = (label k) x
apply (Factor k) x = (factor k) x
apply (Norm) x = norm x
apply Define x = define x
apply Pushin x = pushin x
--apply (Compose m1 m2) x = (apply m2 x) >>= (\y -> apply m1 y)
apply (Extend k0 m) (Prod n k f) | k0==k = Just (Prod n k (\i -> f i >>= (\g -> apply m g)))  --- ( int -> Maybe R ) >>= (Ring -> Maybe Ring)  :: Int -> Maybe Ring
                                 | otherwise = Nothing
apply (Repeat i m) (Quo n j k x) | i==j = (apply m x) >>= (\y -> Just (Quo n j k y))
                                 | otherwise = Nothing
apply IdR x = Just x

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
permuteMatch = factorMatch <+> (Match matchLabel) <+> (Match matchDefine) <+> (Match matchPushin) <+> (Match matchNorm)
normalizeMatch = permuteMatch <+> (Match matchNorm)
morphismMatch :: IO Match
morphismMatch = do { -- IO
  mc <- matchContext;
  let correctMatch = (filter (\x -> fst x==mc ) [("Factor",factorMatch),("Permute",permuteMatch),("Normalize",normalizeMatch)]) in
      if length correctMatch == 0 then (logObj "MATCH_CONTEXT value invalid "  mc) >> (return factorMatch) else return (snd (head (correctMatch))); }

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
    mm <- morphismMatch; -- Match
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
matchFactor (Base n d b p) = return [Factor k | k <-filter (\x -> x <= 32) (non_triv_factors n) ]
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

matchPushin :: Ring -> IO [Morphism]
matchPushin (Quo n kq d0 (Prod n2 kp f)) = return [Pushin]
matchPushin r = return []

matchId :: Ring -> IO [Morphism]
matchId r = return [IdR]

---


---

define_morphism :: Morphism -> Ring -> Maybe (LinearOp FF)

define_morphism (Factor k) (Base n d b p) = Just (phi n k d b p)
define_morphism (Factor k) r = Nothing
define_morphism (Label k) (Base n d b p) = Just (mL n k)
define_morphism (Label k) r = Nothing
define_morphism Norm (Base n d b p) = Just (gamma n d b p)
define_morphism Norm r = Nothing
define_morphism Define (Quo n k d0 (Base 1 d b p)) = Just (mId n)
define_morphism Define r = Nothing
define_morphism Pushin (Quo n kq d0 (Prod n2 kp f)) = Just (mL n kq) -- check -- wrong (uses T)
define_morphism Pushin r = Nothing
define_morphism (Repeat k0 m) (Quo n k1 d r) = if k0 == k1 then (define_morphism m r) >>= (\lo -> (repeatLO n lo)) else Nothing
define_morphism (Repeat k0 m) r = Nothing
define_morphism (Extend k0 m) (Prod n k f) = extendLO n k0 (\i -> (f i) >>= (\r -> define_morphism m r))
define_morphism (Extend k0 m) r = Nothing

morphism_to_kernel :: Morphism -> Ring -> Maybe Kernel
morphism_to_kernel (Factor k) (Base n d b p) = Just (Phi n k d b p)
morphism_to_kernel (Factor k) r = Nothing
morphism_to_kernel (Label k) (Base n d b p) = Just (KL n (div n k))
morphism_to_kernel (Label k) r = Nothing
morphism_to_kernel Norm (Base n d b p) = Just (Gamma n d b p)
morphism_to_kernel Norm r = Nothing
morphism_to_kernel Define (Quo n k d0 (Base 1 d b p)) = Just (KId n)
morphism_to_kernel Define r = Nothing
morphism_to_kernel Pushin (Quo n kq d0 (Prod n2 kp f)) = Just (KT n kq (div n2 kp)) -- check
morphism_to_kernel Pushin r = Nothing
morphism_to_kernel (Repeat k0 m) (Quo n k1 d r) = if k0 == k1 then (morphism_to_kernel m r) >>= (\lo -> Just (Kernel_Repeat n k0 lo)) else Nothing
morphism_to_kernel (Repeat k0 m) r = Nothing
morphism_to_kernel (Extend k0 m) (Prod n k f) = Just (Kernel_Extend n k0 (\i -> (f i) >>= (\r -> morphism_to_kernel m r)))
morphism_to_kernel (Extend k0 m) r = Nothing 

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
