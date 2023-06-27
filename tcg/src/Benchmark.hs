

import Algebra.FField
import Algebra.NTT
import Compile.CompileKernel
import Search.Search
import Algebra.Fourier
import Algebra.PolyRings
import Data.List
import Compile.Compilers
import Util.Logger
import Util.KernelTimer
import Util.Util
import Algebra.FField

import System.Random

--timeSize :: Ring -> IO [(Int,Int,Float)]
--timeSize r = let n = get_size r in
--  let power=round (log n / log 2) in if 2**power/=n then error show n++" is not power of 2" else 
--timeFactorPath :: Ring -> Int -> Int -> Int -> IO Float
factorPath :: Ring -> Int ->  Int -> Int -> IO Path
factorPath r k t l = do { path <- turtles r (Factor k);
                          short_path <- return (takePath t path);
                          turtlesExtend short_path (Factor l) }

listFactorPaths :: Integral a => Int -> [(a,a,a)]
listFactorPaths n = let fn = fromIntegral n in
  let l=foldr (++) [] [[(i,j) | j <-[0..(fn/2/i)]] | i<-[1..fn/2]] :: [(Float,Float)] in
    fmap (\(i,j) -> ( round (2**i),round j,round (((2**i)**j)*(2**(fromIntegral (mod n (round i))))))) l


-- logn -> p -> [(factor,lastfactor,time)]
--timeFactorPaths :: Int -> Int -> IO [(Int,Int,Float)]
timeFactorPaths logn p = let n=round (2**(fromIntegral logn)) in sequence (do { -- []
  (k,t,l) <- listFactorPaths logn;
  return (pure (\x -> (k,l,x)) <*>( (factorPath (Base n (-1) n p) k (-t) l) >>= (\p -> timePath p "Gen"))); })


timeSample :: Ring -> StdGen -> Int -> IO [Float]
timeSample ring gen size =
  do  
    paths <- randomSample ring gen size -- [Path]
    timed <- sequence (fmap time_func  paths) -- [String]
    return timed 
  where
    time_func :: Path -> IO Float
    time_func path = do
      compiled <- compilePath path
      timed <- writeCode "GenTime" compiled >> timeCodeAvg "GenTime"
      return timed

benchmark :: Int -> IO ()
benchmark n = let
  p = splitting_prime n
  in 
  do
    tfp <- timeFactorPaths (log2_round n) p -- [(Int,Int,Float)]
    tsp <- timeSample (Base n 0 n p) (mkStdGen 10) 20 -- [Float]
    logObj "Benchmark: factor paths" tfp >> logObj "Benchmark: random sample" tsp
  
main :: IO ()
main = benchmark 256
