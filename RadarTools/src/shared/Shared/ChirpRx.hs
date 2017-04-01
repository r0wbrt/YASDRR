{-

Copyright 2017 Robert Christian Taylor

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

{- |
Module      :  Shared.ChirpRx
Description :  Program to process received radar chirps
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This program processes a radar signal using correlation to perform chirp
pulse compression
-}
module Shared.ChirpRx where


-- System imports
import System.IO
import System.Exit
import qualified Control.Monad as CM
import Data.Complex
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VUB
import System.Console.GetOpt as GetOpt

--  yasdrr library imports
import qualified YASDRR.SDR.ChirpRadar as Chirp

-- yasdrr executable imports
import qualified Shared.ChirpCommon as ChirpCommon
import qualified Shared.CommandLine as CL
import qualified Shared.IO as SIO


-- | process the command line input into the program settings
processCommandInput :: GetOpt.ArgOrder (ChirpCommon.ChirpOptions -> IO ChirpCommon.ChirpOptions) -> [String] ->  (IO ChirpCommon.ChirpOptions, [String], [String])
processCommandInput argOrder arguments = (CL.processInput ChirpCommon.startOptions actions, extra, errors)
    where (actions, extra, errors) = GetOpt.getOpt argOrder ChirpCommon.chirpRadarRxOptions arguments 


-- | The main IO entry point for standalone RX radar.
chirpRxMainIO :: [String] -> IO ()
chirpRxMainIO arguments =
    case processCommandInput GetOpt.RequireOrder arguments of
        (programSettingsIO, [], []) -> do
                
                programSettings <- programSettingsIO
                
                let errorCheck = CL.validateOptions programSettings ChirpCommon.chirpRadarRxValidators
             
                CM.when (errorCheck /= []) (CL.programInputError errorCheck)
                
                hSetBinaryMode stdout True 
                hSetBinaryMode stdin True
                
                chirpRxMain programSettings
                
                hSetBinaryMode stdout False 
                hSetBinaryMode stdin False 
                
                _ <- ChirpCommon.optCloseOutput programSettings
                _ <- ChirpCommon.optCloseInput programSettings
                
                exitSuccess
        (_, _, errors) -> CL.programInputError errors


-- | Emebeded mode entry point for chirp radar rx program
chirpRxMain :: ChirpCommon.ChirpOptions -> IO () 
chirpRxMain programSettings = do
              
              let chirpLength = ChirpCommon.calculateSignalLength programSettings
              let outputFormat = ChirpCommon.optOutputSampleFormat programSettings
              let inputFormat = ChirpCommon.optInputSampleFormat programSettings
              
              let silenceLength = ChirpCommon.optSilenceLength programSettings
              
              let pulseTruncationLength = ChirpCommon.optSilenceTruncateLength programSettings
              
              let inputReader = ChirpCommon.optInputReader programSettings
              let signalReader = readInput (floor chirpLength + silenceLength) pulseTruncationLength inputFormat inputReader
               
              let signalWriter signal = ChirpCommon.optOutputWriter programSettings $ SIO.serializeOutput outputFormat signal
              
              let chirpSettings = Chirp.ChirpRadarSettings
                    { Chirp.optStartFrequency = ChirpCommon.optStartFrequency programSettings
                    , Chirp.optEndFrequency = ChirpCommon.optEndFrequency programSettings
                    , Chirp.optFrequencyShift = ChirpCommon.optFrequencyShift programSettings
                    , Chirp.optSampleRate = ChirpCommon.optSampleRate programSettings
                    , Chirp.optRiseTime = chirpLength
                    , Chirp.optSilenceLength = silenceLength
                    , Chirp.optSilenceTruncateLength = pulseTruncationLength
                    , Chirp.optAmplitude = ChirpCommon.optAmplitude programSettings
                    , Chirp.optChirpWindow = ChirpCommon.optChirpWindow programSettings
                    , Chirp.optSignalWindow = ChirpCommon.optSignalWindow programSettings
                    }
              
              let signalProcessor = Chirp.chirpRx chirpSettings
              
              processData signalProcessor signalReader signalWriter


-- | Compresses a received radar signal using pulse compression.
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


-- | Reads in radar samples using the reader function
readInput :: Int -> Int -> CL.SampleFormat -> (Int -> IO B.ByteString) -> IO (Maybe (VUB.Vector (Complex Double)))
readInput signalLength pulseTruncationLength sampleFormat reader = do    
        
    fileBlock <- reader signalLengthBytes
    
    return $ if B.length fileBlock < signalLengthBytes then Nothing 
                    else Just $ deserializer (B.take (B.length fileBlock - truncationLengthBytes) fileBlock) 
    
    where sampleSize = case sampleFormat of
                        CL.SampleComplexDouble -> 16
                        CL.SampleComplexFloat -> 8
                        CL.SampleComplexSigned16 -> 4
          
          signalLengthBytes = signalLength * sampleSize
          truncationLengthBytes = sampleSize * pulseTruncationLength
          
          deserializer bString = VUB.fromList $ fst $ SIO.deserializeBlock decoder bString
            where decoder = case sampleFormat of
                                CL.SampleComplexDouble -> SIO.complexDoubleDeserializer
                                CL.SampleComplexFloat -> SIO.complexFloatDeserializer
                                CL.SampleComplexSigned16 -> SIO.complexSigned16DeserializerOne
