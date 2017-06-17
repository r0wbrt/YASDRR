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
module Shared.ChirpRx
    ( processCommandInput
    , chirpRxMainIO
    , chirpRxMain
    ) where


-- System imports
import qualified Control.Monad         as CM
import qualified Data.ByteString       as B
import           System.Console.GetOpt as GetOpt
import           System.Exit
import           System.IO

--  yasdrr library imports
import qualified YASDRR.SDR.ChirpRadar as Chirp

-- yasdrr executable imports
import qualified Shared.ChirpCommon    as ChirpCommon
import qualified Shared.CommandLine    as CL
import qualified Shared.IO             as SIO


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

              let signalWriter = ChirpCommon.optOutputWriter programSettings

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
                    }

              let signalProcessor input = SIO.serializeOutput outputFormat $ Chirp.chirpRx chirpSettings $ SIO.deserializeInput inputFormat input

              processData signalProcessor signalReader signalWriter

              return ()


processData :: (B.ByteString -> B.ByteString) ->
                IO (Maybe B.ByteString) -> (B.ByteString -> IO ())-> IO ()
processData signalProcessor signalReader signalWriter = do

    fileBlockWrapper <- signalReader

    case fileBlockWrapper of
         Just fileBlock -> do
             let res = signalProcessor fileBlock
             signalWriter res
             processData signalProcessor signalReader signalWriter
         Nothing -> return ()


-- | Reads in radar samples using the reader function
readInput :: Int -> Int -> CL.SampleFormat -> (Int -> IO B.ByteString)
                -> IO (Maybe B.ByteString)
readInput signalLength pulseTruncationLength sampleFormat reader = do

    fileBlock <- reader signalLengthBytes

    return $! if B.length fileBlock < signalLengthBytes then Nothing
                    else Just $ B.take (B.length fileBlock - truncationLengthBytes) fileBlock

    where sampleSize = case sampleFormat of
                        CL.SampleComplexDouble -> 16
                        CL.SampleComplexFloat -> 8
                        CL.SampleComplexSigned16 -> 4
                        _ -> error "Sample format not supported"

          signalLengthBytes = signalLength * sampleSize
          truncationLengthBytes = sampleSize * pulseTruncationLength
