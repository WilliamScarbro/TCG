{-# LANGUAGE ScopedTypeVariables #-}

module Search.Annealing where


import Control.Monad.State.Strict
import System.Random

import Search.Search
import Algebra.FField
import Algebra.PolyRings
import Algebra.NTT
import Algebra.Fourier
import Util.Logger
import Util.Util
import Util.KernelTimer

import Control.Monad
import System.IO.Unsafe
import qualified Data.Map as Map (empty,insert,Map,member,mapAccumWithKey,lookup,fromList,toList,update)
import qualified Rando as Rando (shuffle)

class AnnealingEnv a where
  update_env_with_temp :: Float -> a -> a
  
type Cache = Map.Map String Float
type AnnealingState a = (Float, Path, Cache, a, [(Path, Float)], Path)

simulatedAnnealing :: forall a. AnnealingEnv a =>
  (a -> Path -> IO (a, [Path])) ->
  (String -> IO Float) ->
  (Path -> IO String) ->
  a ->
  Path ->
  IO ([(Path,Float)],Path)
simulatedAnnealing expand heuristic compile initEnv init = evalStateT go initialState
  where
    initialState :: AnnealingState a
    initialState = (1, init, Map.empty, initEnv, [], init)

    n = get_size . path_get_start $ init
    
    -- Main simulated annealing loop
    go :: StateT (AnnealingState a) IO ([(Path,Float)],Path)
    go = do
      (temp, state, cache, env, history, bestState) <- get
      if temp <= 0
        then return (reverse history,bestState)
        else do -- StateT
          -- Expand state and choose random neighbor
          let expand_io = expand env state
          ((newEnv,neighbors),expand_time) <- ligtIO $ timeCodeBlock expand_io

          let neighbors_with_current = if null neighbors then [state] else neighbors
          newStateIndex <- liftIO $ randomRIO (0, length neighbors_with_current - 1)
          let newState = neighbors_with_current !! newStateIndex

          -- Find costs for current and new state

          let io_newCost = findWithCache newState cache heuristic compile
          (newCost,newCost_time) <- ligtIO $ timeCodeBlock io_newCost
          
          cost <- liftIO $ findWithCache state cache heuristic compile


          -- Update cache
          let cache' = Map.insert (unsafePerformIO $ compile newState) newCost cache

          -- Calculate acceptance probability
          let diff = cost - newCost
          prob <- liftIO $ randomRIO (0, 1)
          let acceptance = if diff > 0 then 1 else exp (diff / temp)

          -- Cool the system
          let temp' = cool temp

          -- Update newEnv with new temp
          let newEnv' = update_env_with_temp temp' newEnv 

          -- Determine next state (acceptence dependent)
          (state', cache'', env', history', bestState')
            <- getNextState prob acceptance newState newCost state cost cache' newEnv' history bestState
          
          -- Randomly restart search from best state so far
          restart <- liftIO $ randomRIO (0 :: Float, 1)
          let restart_trigger = restart < 0.05
          let (state'', env'') = if restart_trigger then (bestState', env) else (state', env')
          liftIO $ if restart_trigger then logObj "Restarting" "" else return ()

          -- Log current annealing state
          liftIO $ logObj "Annealing State" (state'', temp', newCost) 
          liftIO $ logObj "Times newCost expand" (newCost_time,expand_time)
          
          -- Update state
          put (temp', state'', cache'', env'', history', bestState')
          go

    -- Determine the next state of the system based on acceptance probability
    getNextState :: 
      Float -> Float ->
      Path -> Float -> Path -> Float -> Cache -> a -> [(Path, Float)] -> Path ->
      StateT (AnnealingState a) IO (Path, Cache, a, [(Path, Float)], Path)
    getNextState prob acceptance newState newCost state cost cache' newEnv history bestState = 
      if prob < acceptance 
        then return (newState, cache', newEnv, (newState, newCost):history, if null history then newState else if newCost < snd (head history) then newState else bestState)
        else return (state, cache', newEnv, (state, cost):history, bestState)

    -- Cooling function (linear cooling)
    cool t = t - 1 / 250  -- Cooling schedule

findWithCache :: Path -> Cache -> (String -> IO Float) -> (Path -> IO String) -> IO Float
findWithCache state cache heuristic compile = do
  compiledState <- compile state
  case Map.lookup compiledState cache of
    Just cost -> return cost
    Nothing -> heuristic compiledState
