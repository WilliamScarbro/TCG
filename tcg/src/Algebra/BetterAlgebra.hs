module Algebra.BetterAlgebra where

class IntRing a where
  add :: a -> Int -> Int -> Int
  sub :: a -> Int -> Int -> Int
  mult :: a -> Int -> Int -> Int
  pow :: a -> Int -> Int -> Int
  equal :: a -> Int -> Int -> Bool
  
class (IntRing a) => IntField a where
  inverse :: a -> Int -> Int
  get_generator :: a -> Int
  

data ModPrime = ModPrime Int Int deriving Show

init_ModPrime :: Int -> ModPrime
init_ModPrime prime = ModPrime prime (find_generator prime)

find_generator p =
    let
      gens = filter (\x -> length x == p-1) . fmap (generate (ModPrime p 0)) $ [2..p-1]
    in
      if null gens then
        error ("No generator found: "++show p)
      else
        (head gens) !! 0

get_mod (ModPrime p _) = p

mod_operator :: ModPrime -> (Int -> Int -> Int) -> (Int -> Int -> Int)
mod_operator (ModPrime p _) op = (\x y -> mod (op x y) p)


instance IntRing ModPrime where
  add mp = mod_operator mp (+)
  sub mp = mod_operator mp (-)
  mult mp = mod_operator mp (*)
  equal (ModPrime p _) x y = mod x p == mod y p
  pow mp base exp =
    pow_help 1 base exp
    where
      pow_help y x 0 = y
      pow_help y x e | mod e 2 == 1 =
                       pow_help res sq (div e 2)
                     | mod e 2 == 0 =
                       pow_help y sq (div e 2)
        where
          sq = mult mp x x
          res = mult mp y x

-- the generator in mp is wrong and that's ok (it hasn't been found yet)
generate :: ModPrime -> Int -> [Int]
generate mp n = generate_help mp n n
  where
    generate_help mp n 0 = error (show mp++" is not prime")
    generate_help mp n 1 = [1]
    generate_help mp n x = x:(generate_help mp n (mult mp n x))

instance IntField ModPrime where
  get_generator (ModPrime _ g) = g
  inverse mp x | equal mp x 0 = error "attempting to find inverse of zero"
               | otherwise = snd . head . filter (\(x,y) -> equal mp (mult mp x y) 1) $ [(x,i) | i<-[1..(get_mod mp)-1]]
               
get_nth_root mp n | mod ((get_mod mp)-1) n /= 0 = error ("get_nth_root: "++show n++" does not divide "++show ((get_mod mp)-1))
                  | otherwise = pow mp (get_generator mp) (div ((get_mod mp)-1) n)  
       
-- inverse mp x | equal x 0 = Nothing
--                | otherwise = let p=get_mod x in Just (fst ( head ( filter (\(y,z) -> z==Just one) [(y,x `mult` y) | y<-[Res i p| i<-[1..p-1]]] ) ) )
  
    

data ModPrimeMemo = MPM {prime :: Int, generator :: Int, n :: Int, nth_root :: Int, root_power_table :: [Int] } deriving Show


init_ModPrimeMemo p n =
  let
    modPrime = init_ModPrime p
    gen = get_generator modPrime
    nr = get_nth_root modPrime n
    power_table :: ModPrime -> Int -> Int -> Int -> [Int]
    power_table mp x y 0 = [y]
    power_table mp x y i = y:(power_table mp x (mult mp x y) (i-1))
    rpt = power_table modPrime nr 1 (n-1)
  in
    MPM p gen n nr rpt

mpm_to_mp mpm = ModPrime (prime mpm) (generator mpm)

instance IntRing ModPrimeMemo where
  add mpm = add (mpm_to_mp mpm)
  sub mpm = sub (mpm_to_mp mpm)
  mult mpm = mult (mpm_to_mp mpm)
  equal mpm = equal (mpm_to_mp mpm)
  pow mpm = pow (mpm_to_mp mpm)
  
instance IntField ModPrimeMemo where
  get_generator mpm = generator mpm
  inverse mpm = inverse (mpm_to_mp mpm)


get_root_power :: ModPrimeMemo -> Int -> Int
get_root_power mpm e = (root_power_table mpm) !! (mod e (n mpm))
