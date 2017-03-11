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


import qualified Options as Opts
import System.Environment
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import qualified YASDRR.Recipes.ChirpTx as ChirpTx
import qualified Data.ByteString as B
import qualified YASDRR.Recipes.SharedRecipesOptions as SROptions

main :: IO ()
{-# ANN module "HLint: ignore Use :" #-}
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder Opts.chirpRadarTxOptions arguments of
        (actions, [], []) -> do
              programSettings <- Opts.processInput actions
              
              hSetBinaryMode stdout True 
              
              let chirpLength = Opts.calculateSignalLength programSettings
              let repetitions = Opts.optRepetitions programSettings
              
              let outputFormat = Opts.optOutputSampleFormat programSettings
              
              
              let chirpSettings = SROptions.ChirpRadarSettings
                    { SROptions.optStartFrequency = Opts.optStartFrequency programSettings
                    , SROptions.optEndFrequency = Opts.optEndFrequency programSettings
                    , SROptions.optFrequencyShift = Opts.optFrequencyShift programSettings
                    , SROptions.optSampleRate = Opts.optSampleRate programSettings
                    , SROptions.optRiseTime = chirpLength
                    , SROptions.optSilenceLength = Opts.optSilenceLength programSettings
                    , SROptions.optSilenceTruncateLength = Opts.optSilenceTruncateLength programSettings
                    , SROptions.optAmplitude = Opts.optAmplitude programSettings
                    , SROptions.optChirpWindow = Opts.optChirpWindow programSettings
                    , SROptions.optSignalWindow = Opts.optSignalWindow programSettings
                    }
              
              let finalSignal = Opts.serializeOutput outputFormat $ ChirpTx.main chirpSettings
              
              let writer = Opts.optOutputWriter programSettings
              
              _ <- writeOutput writer finalSignal repetitions
              
              _ <- Opts.optCloseOutput programSettings
              
              hSetBinaryMode stdout False 
              
              exitSuccess
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure
              
              
writeOutput :: (B.ByteString -> IO ()) -> B.ByteString -> Int -> IO ()
writeOutput _ _ 0 = return ()

writeOutput writer signal count = do
    
    let newCount = if count /= -1 then count - 1 else -1
    
    writer signal
    writeOutput writer signal newCount

