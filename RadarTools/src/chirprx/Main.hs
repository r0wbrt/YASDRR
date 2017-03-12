--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  ChirpRx
Description :  Program to process radar chirps
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This program performs pulse compression on received chirp radar pulses.
-}

import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt as GetOpt
import qualified Shared.ChirpRx as ChirpRx
import qualified Shared.ChirpCommon as Opts

main :: IO ()
{-# ANN module "HLint: ignore Use :" #-}
main = do
    arguments <- getArgs
    case ChirpRx.processCommandInput GetOpt.RequireOrder arguments of
        (programSettingsIO, [], []) -> do
              
              programSettings <- programSettingsIO
              
              hSetBinaryMode stdout True 
              hSetBinaryMode stdin True
              
              ChirpRx.chirpRxMain programSettings
              
              _ <- Opts.optCloseOutput programSettings
              _ <- Opts.optCloseInput programSettings
              
              hSetBinaryMode stdout False 
              hSetBinaryMode stdin False 
              
              exitSuccess
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure

