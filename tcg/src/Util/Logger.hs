module Util.Logger where

import Control.Monad

logging=True
loglocation = "log/polymult.log"

clearLog :: IO ()
clearLog = writeFile loglocation ""

logObj :: (Show a) => String -> a -> IO ()
logObj label x = if logging then appendFile loglocation (label++": "++(show x)++"\n") else return ()

logIO :: (Show a) => String -> IO a -> IO ()
logIO label x = join . fmap (logObj label) $ x
