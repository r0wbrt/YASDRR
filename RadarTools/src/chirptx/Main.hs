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
import qualified YASDRR.SDR.ChirpRadar as Chirp
import qualified Data.Vector.Unboxed as V
import Data.Complex
import qualified Data.ByteString as B


main :: IO ()
{-# ANN module "HLint: ignore Use :" #-}
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder Opts.chirpRadarTxOptions arguments of
        (actions, [], []) -> do
              programSettings <- Opts.processInput actions
              
              hSetBinaryMode stdout True 
              
              let sampleRate = Opts.optSampleRate programSettings
              
              let frequecyShift = Opts.optFrequencyShift programSettings
              let startFrequency =frequecyShift +  Opts.optStartFrequency programSettings
              let endFrequency = frequecyShift + Opts.optEndFrequency programSettings
              
              let amplitude = Opts.optAmplitude programSettings
              
              let chirpLength = Opts.calculateSignalLength programSettings
              let repetitions = Opts.optRepetitions programSettings
              let silenceLength = Opts.optSilenceLength programSettings
              
              let outputFormat = Opts.optOutputSampleFormat programSettings
              
              let normalizedChirp = Chirp.generateChirp sampleRate startFrequency endFrequency chirpLength
              let window = Opts.getChirpWindow (Opts.optChirpWindow programSettings) $ floor chirpLength 
              let adjustedChirp = V.map (\sample -> (amplitude :+ 0.0) * sample ) normalizedChirp
              let windowedChirp = V.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window adjustedChirp
              
              let finalSignal = Opts.serializeOutput outputFormat $ windowedChirp V.++ V.replicate silenceLength (0.0 :+ 0.0)
              
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

