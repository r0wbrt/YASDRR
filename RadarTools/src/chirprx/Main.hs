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
import Data.Complex
import qualified Data.ByteString as B
import qualified Options as Opts
import qualified Data.Vector.Unboxed as VUB
import qualified YASDRR.IO.ComplexSerialization as IOComplex
import qualified YASDRR.Recipes.ChirpRx as ChirpRx
import qualified YASDRR.Recipes.SharedRecipesOptions as SROptions

main :: IO () 
{-# ANN module "HLint: ignore Use :" #-}
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder Opts.chirpRadarRxOptions arguments of
        (actions, [], []) -> do
              programSettings <- Opts.processInput actions
              
              hSetBinaryMode stdout True 
              hSetBinaryMode stdin True 
              
              let chirpLength = Opts.calculateSignalLength programSettings
              let outputFormat = Opts.optOutputSampleFormat programSettings
              let inputFormat = Opts.optInputSampleFormat programSettings
              
              let silenceLength = Opts.optSilenceLength programSettings
              
              let pulseTruncationLength = Opts.optSilenceTruncateLength programSettings
              
              let inputReader = Opts.optInputReader programSettings
              let signalReader = readInput (floor chirpLength + silenceLength) pulseTruncationLength inputFormat inputReader
               
              let signalWriter signal = Opts.optOutputWriter programSettings $ Opts.serializeOutput outputFormat signal
              
              let chirpSettings = SROptions.ChirpRadarSettings
                    { SROptions.optStartFrequency = Opts.optStartFrequency programSettings
                    , SROptions.optEndFrequency = Opts.optEndFrequency programSettings
                    , SROptions.optFrequencyShift = Opts.optFrequencyShift programSettings
                    , SROptions.optSampleRate = Opts.optSampleRate programSettings
                    , SROptions.optRiseTime = chirpLength
                    , SROptions.optSilenceLength = silenceLength
                    , SROptions.optSilenceTruncateLength = pulseTruncationLength
                    , SROptions.optAmplitude = Opts.optAmplitude programSettings
                    , SROptions.optChirpWindow = Opts.optChirpWindow programSettings
                    , SROptions.optSignalWindow = Opts.optSignalWindow programSettings
                    }
              
              let signalProcessor = ChirpRx.main chirpSettings
              
              _ <- processData signalProcessor signalReader signalWriter
              
              _ <- Opts.optCloseOutput programSettings
              _ <- Opts.optCloseInput programSettings 
              
              hSetBinaryMode stdout False
              hSetBinaryMode stdin False 
              
              exitSuccess
              
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure
                                
                                
processData :: (VUB.Vector (Complex Double) ->
                VUB.Vector (Complex Double)) -> 
                 IO (Maybe ( VUB.Vector (Complex Double) ) )  -> 
                  (VUB.Vector (Complex Double) -> IO ()) -> IO ()
processData signalProcessor signalReader signalWriter = do
    
    fileInput <- signalReader
    
    case fileInput of
         Just radarReturn -> do
             signalWriter $ signalProcessor radarReturn
             processData signalProcessor signalReader signalWriter
         Nothing -> return ()
         
         
readInput :: Int -> Int -> Opts.SampleFormat -> (Int -> IO B.ByteString) -> IO (Maybe (VUB.Vector (Complex Double)))
readInput signalLength pulseTruncationLength sampleFormat reader = do    
        
    fileBlock <- reader signalLengthBytes
    
    return $ if B.length fileBlock < signalLengthBytes then Nothing 
                    else Just $ deserializer (B.take (B.length fileBlock - truncationLengthBytes) fileBlock) 
    
    where sampleSize = case sampleFormat of
                        Opts.SampleComplexDouble -> 16
                        Opts.SampleComplexFloat -> 8
                        Opts.SampleComplexSigned16 -> 4
          
          signalLengthBytes = signalLength * sampleSize
          truncationLengthBytes = sampleSize * pulseTruncationLength
          
          deserializer bString = VUB.fromList $ fst $ IOComplex.deserializeBlock decoder bString
            where decoder = case sampleFormat of
                                Opts.SampleComplexDouble -> IOComplex.complexDoubleDeserializer
                                Opts.SampleComplexFloat -> IOComplex.complexFloatDeserializer
                                Opts.SampleComplexSigned16 -> IOComplex.complexSigned16Deserializer 1.0



