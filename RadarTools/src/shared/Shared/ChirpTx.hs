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
Module      :  Shared.ChirpTx
Description :  Program to generate radar chirps
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable
Portability :  portable

This program generates linear chirps for chirp based pulse compression radar.
-}
module Shared.ChirpTx
    ( processCommandInput
    , chirpTxMainIO
    , chirpTxMain
    ) where


-- System imports
import qualified Control.Monad         as CM
import qualified Data.ByteString       as B
import           System.Console.GetOpt as GetOpt
import           System.Exit
import           System.IO

-- yasdrr imports
import qualified YASDRR.SDR.ChirpRadar as Chirp

-- yasdrr shared imports
import qualified Shared.ChirpCommon    as ChirpCommon
import qualified Shared.CommandLine    as CL
import qualified Shared.IO             as SIO

-- | Process the command line input and populates the setting record.
processCommandInput :: GetOpt.ArgOrder (ChirpCommon.ChirpOptions -> IO ChirpCommon.ChirpOptions) -> [String] ->  (IO ChirpCommon.ChirpOptions, [String], [String])
processCommandInput argOrder arguments = (CL.processInput ChirpCommon.startOptions actions, extra, errors)
    where (actions, extra, errors) = GetOpt.getOpt argOrder ChirpCommon.chirpRadarTxOptions arguments


-- | Chirp TX stanalone mode entry.
chirpTxMainIO :: [String] -> IO ()
chirpTxMainIO arguments =
    case processCommandInput GetOpt.RequireOrder arguments of
        (programSettingsIO, [], []) -> do

              programSettings <- programSettingsIO

              let errorCheck = CL.validateOptions programSettings ChirpCommon.chirpRadarTxValidators

              CM.when (errorCheck /= []) (CL.programInputError errorCheck)


              hSetBinaryMode stdout True

              chirpTxMain programSettings

              hSetBinaryMode stdout False

              _ <- ChirpCommon.optCloseOutput programSettings

              exitSuccess
        (_, _, errors) -> CL.programInputError errors


-- | Chirp TX emebeded main entry.
chirpTxMain :: ChirpCommon.ChirpOptions -> IO ()
chirpTxMain programSettings = do

    let chirpLength = ChirpCommon.calculateSignalLength programSettings
    let repetitions = ChirpCommon.optRepetitions programSettings

    let outputFormat = ChirpCommon.optOutputSampleFormat programSettings

    let chirpSettings = Chirp.ChirpRadarSettings
            { Chirp.optStartFrequency = ChirpCommon.optStartFrequency programSettings
            , Chirp.optEndFrequency = ChirpCommon.optEndFrequency programSettings
            , Chirp.optFrequencyShift = ChirpCommon.optFrequencyShift programSettings
            , Chirp.optSampleRate = ChirpCommon.optSampleRate programSettings
            , Chirp.optRiseTime = chirpLength
            , Chirp.optSilenceLength = ChirpCommon.optSilenceLength programSettings
            , Chirp.optSilenceTruncateLength = ChirpCommon.optSilenceTruncateLength programSettings
            , Chirp.optAmplitude = ChirpCommon.optAmplitude programSettings
            , Chirp.optChirpWindow = ChirpCommon.optChirpWindow programSettings
            }

    let finalSignal = SIO.serializeOutput outputFormat $ Chirp.chirpTx chirpSettings

    let writer = ChirpCommon.optOutputWriter programSettings

    writeOutput writer finalSignal repetitions


-- | Writes the output of the chirp tx program
writeOutput :: (B.ByteString -> IO ()) -> B.ByteString -> Int -> IO ()
writeOutput _ _ 0 = return ()
writeOutput writer signal count = do

    let newCount = if count /= -1 then count - 1 else -1

    writer signal
    writeOutput writer signal newCount
