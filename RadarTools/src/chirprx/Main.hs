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
import Data.Maybe
import qualified Data.ByteString as B
import qualified Options as Opts
import qualified Data.Vector as V
import qualified YASDRR.SDR.ChirpRadar as Chirp
import qualified YASDRR.DSP.Correlation as Cor
import qualified YASDRR.IO.ComplexSerialization as IOComplex
import qualified YASDRR.DSP.Windows as Windows

main :: IO () 
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder Opts.chirpRadarRxOptions arguments of
        (actions, [], []) -> do
              programSettings <- Opts.processInput actions
              
              hSetBinaryMode stdout True 
              hSetBinaryMode stdin True 
              
              let sampleRate = Opts.optSampleRate programSettings
              
              let frequecyShift = Opts.optFrequencyShift programSettings
              let startFrequency = frequecyShift +  Opts.optStartFrequency programSettings
              let endFrequency = frequecyShift + Opts.optEndFrequency programSettings
              
              let chirpLength = Opts.calculateSignalLength programSettings
              let silenceLength = Opts.optSilenceLength programSettings
              
              let outputFormat = Opts.optOutputSampleFormat programSettings
              let inputFormat = Opts.optInputSampleFormat programSettings
              
              let normalizedChirp = Chirp.generateChirp sampleRate startFrequency endFrequency chirpLength
              
              let window = Opts.getChirpWindow (Opts.optChirpWindow programSettings) $ floor chirpLength 
              let windowedChirp = V.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window normalizedChirp
              
              let pulseTruncationLength = Opts.optSilenceTruncateLength programSettings
              let inputReader = Opts.optInputReader programSettings
              let signalReader = readInput (floor chirpLength + silenceLength) pulseTruncationLength inputFormat inputReader
              
              let signalWriter signal = Opts.optOutputWriter programSettings $ Opts.serializeOutput outputFormat signal
              
              let pulseWindow = getPulseWindow (Opts.optSignalWindow programSettings) (floor chirpLength + silenceLength - pulseTruncationLength)
              
              _ <- processData windowedChirp pulseWindow signalReader signalWriter
              
              _ <- Opts.optCloseOutput programSettings
              _ <- Opts.optCloseInput programSettings
              
              hSetBinaryMode stdout False
              hSetBinaryMode stdin False 
              
              exitSuccess
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure
              
              
compressReturn :: V.Vector (Complex Double) -> Maybe (V.Vector (Complex Double)) -> 
                        V.Vector (Complex Double) -> V.Vector (Complex Double)
compressReturn chirp pulseWindow signal = Cor.correlateV chirp windowedSignal

    where windowedSignal = if isNothing pulseWindow then 
                            signal 
                                else V.zipWith (*) (fromJust pulseWindow) signal
                                
                                
processData :: V.Vector (Complex Double) ->
                Maybe (V.Vector (Complex Double)) -> 
                 (IO (Maybe ( V.Vector (Complex Double) ) ) ) -> 
                  (V.Vector (Complex Double) -> IO ()) -> IO ()
processData chirp pulseWindow signalReader signalWriter = do
    
    fileInput <- signalReader
    
    case fileInput of
         Just radarReturn -> do
             let compressedReturn = compressReturn chirp pulseWindow radarReturn
             signalWriter compressedReturn
             processData chirp pulseWindow signalReader signalWriter
         Nothing -> return ()
         
         
readInput :: Int -> Int -> Opts.SampleFormat -> (Int -> IO B.ByteString) -> IO (Maybe (V.Vector (Complex Double)))
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
          
          deserializer bString = V.fromList $ fst $ IOComplex.deserializeBlock decoder bString
            where decoder = case sampleFormat of
                                Opts.SampleComplexDouble -> IOComplex.complexDoubleDeserializer
                                Opts.SampleComplexFloat -> IOComplex.complexFloatDeserializer
                                Opts.SampleComplexSigned16 -> IOComplex.complexSigned16Deserializer 1.0
          
          
getPulseWindow :: Opts.SignalWindow -> Int -> Maybe (V.Vector (Complex Double))
getPulseWindow window n = case window of
                          Opts.HammingWindow -> Just $ V.map (\s -> s :+ 0) $ Windows.hammingWindowV n
                          Opts.NoWindow -> Nothing


