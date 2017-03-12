

module Shared.CommandLine where

processInput :: a -> [a -> IO a] -> IO a
processInput startOptions = foldl (>>=) (return startOptions) 
