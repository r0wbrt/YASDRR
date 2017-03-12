--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  ChirpTx
Description :  Program to generate radar chirps
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This program generates linear chirps for chirp based pulse compression radar.
-}


import qualified Shared.ChirpCommon as Opts
import System.Environment
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import qualified Shared.ChirpTx as ChirpTx

main :: IO ()
{-# ANN module "HLint: ignore Use :" #-}
main = do
    arguments <- getArgs
    case ChirpTx.processCommandInput GetOpt.RequireOrder arguments of
        (programSettingsIO, [], []) -> do
              
              programSettings <- programSettingsIO
              
              hSetBinaryMode stdout True 
              
              ChirpTx.chirpTxMain programSettings
              
              _ <- Opts.optCloseOutput programSettings
              
              hSetBinaryMode stdout False 
              
              exitSuccess
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure
