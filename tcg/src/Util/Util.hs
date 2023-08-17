module Util.Util where

import Util.Logger
import System.Random

maybeToIO :: String -> Maybe a -> IO a
maybeToIO _ (Just m_obj) = return m_obj
maybeToIO str Nothing = logObj "ERROR" str >> (return undefined)

randomChoice :: RandomGen g => [a] -> g -> (a,g)
randomChoice list rand = let (ind,rand2)=randomR (0,(length list)-1) rand in (list!!ind,rand2)

showTuple :: Show a => [a] -> String
showTuple t = let st = fmap show t in foldl (\x y -> (x++","++y)) (head st) (tail st)

showStrTuple :: [String] -> String
showStrTuple t =
  foldl (\x y -> (x++","++y)) (head t) (tail t)

log2_round :: Int -> Int
log2_round x = round ((log (fromIntegral x)) / (log 2))

replaceAt :: Int -> Int -> [[a]] -> [a] -> [[a]]
replaceAt _ _ _ [] = []
replaceAt from to r xs =
  let
    prefix = take from xs
    suffix = drop to xs
  in
    [prefix ++ r_el ++ suffix |r_el<-r]

timeCodeBlock :: IO a -> IO (a, NominalDiffTime)
timeCodeBlock action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    return (result, elapsedTime)
