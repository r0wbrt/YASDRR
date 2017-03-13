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
module Shared.ChirpTx where


import qualified Shared.ChirpCommon as ChirpCommon
import System.Console.GetOpt as GetOpt
import qualified YASDRR.Recipes.ChirpTx as ChirpTx
import qualified Data.ByteString as B
import qualified YASDRR.Recipes.SharedRecipesOptions as SROptions
import qualified Shared.CommandLine as CL
import System.IO
import System.Exit


processCommandInput :: GetOpt.ArgOrder (ChirpCommon.ChirpOptions -> IO ChirpCommon.ChirpOptions) -> [String] ->  (IO ChirpCommon.ChirpOptions, [String], [String])
processCommandInput argOrder arguments = (CL.processInput ChirpCommon.startOptions actions, extra, errors)
    where (actions, extra, errors) = GetOpt.getOpt argOrder ChirpCommon.chirpRadarTxOptions arguments


chirpTxMainIO :: [String] -> IO ()
{-# ANN module "HLint: ignore Use :" #-}
chirpTxMainIO arguments = do
    case processCommandInput GetOpt.RequireOrder arguments of
        (programSettingsIO, [], []) -> do
              
              programSettings <- programSettingsIO
              
              hSetBinaryMode stdout True 
              
              chirpTxMain programSettings
              
              _ <- ChirpCommon.optCloseOutput programSettings
              
              hSetBinaryMode stdout False 
              
              exitSuccess
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure


chirpTxMain :: ChirpCommon.ChirpOptions -> IO ()
{-# ANN module "HLint: ignore Use :" #-}
chirpTxMain programSettings = do
    
    let chirpLength = ChirpCommon.calculateSignalLength programSettings
    let repetitions = ChirpCommon.optRepetitions programSettings
    
    let outputFormat = ChirpCommon.optOutputSampleFormat programSettings
    
    let chirpSettings = SROptions.ChirpRadarSettings
            { SROptions.optStartFrequency = ChirpCommon.optStartFrequency programSettings
            , SROptions.optEndFrequency = ChirpCommon.optEndFrequency programSettings
            , SROptions.optFrequencyShift = ChirpCommon.optFrequencyShift programSettings
            , SROptions.optSampleRate = ChirpCommon.optSampleRate programSettings
            , SROptions.optRiseTime = chirpLength
            , SROptions.optSilenceLength = ChirpCommon.optSilenceLength programSettings
            , SROptions.optSilenceTruncateLength = ChirpCommon.optSilenceTruncateLength programSettings
            , SROptions.optAmplitude = ChirpCommon.optAmplitude programSettings
            , SROptions.optChirpWindow = ChirpCommon.optChirpWindow programSettings
            , SROptions.optSignalWindow = ChirpCommon.optSignalWindow programSettings
            }
    
    let finalSignal = ChirpCommon.serializeOutput outputFormat $ ChirpTx.main chirpSettings
    
    let writer = ChirpCommon.optOutputWriter programSettings
    
    writeOutput writer finalSignal repetitions


writeOutput :: (B.ByteString -> IO ()) -> B.ByteString -> Int -> IO ()
writeOutput _ _ 0 = return ()
writeOutput writer signal count = do
    
    let newCount = if count /= -1 then count - 1 else -1
    
    writer signal
    writeOutput writer signal newCount 
