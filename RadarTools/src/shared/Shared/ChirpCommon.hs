--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  Options
Description :  Shared options for the chirp generator and receiver.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

Common functionlaity between the chirp transmitter and receiver that does not
need to be redistributed as a library.
-}

module Shared.ChirpCommon where

import System.Console.GetOpt as GetOpt
import System.IO
import YASDRR.Recipes.SharedRecipesOptions (SignalWindow (..) )
import qualified Data.Char as DChar
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VUB
import qualified YASDRR.DSP.Windows as Windows
import qualified Shared.CommandLine as CL

data RiseUnits = RiseUnitsSeconds | RiseUnitsSamples

data ChirpOptions = ChirpOptions 
 { optStartFrequency :: Double
 , optEndFrequency :: Double
 , optFrequencyShift :: Double
 , optSampleRate :: Double
 , optRiseTime :: Double
 , optRiseUnit :: RiseUnits
 , optInputSampleFormat :: CL.SampleFormat
 , optOutputSampleFormat :: CL.SampleFormat
 , optInputReader :: Int -> IO B.ByteString
 , optOutputWriter :: B.ByteString -> IO ()
 , optSilenceLength :: Int
 , optSilenceTruncateLength :: Int
 , optRepetitions :: Int
 , optAmplitude :: Double
 , optChirpWindow :: SignalWindow
 , optSignalWindow :: SignalWindow
 , optCloseInput :: IO ()
 , optCloseOutput :: IO ()
 } 


startOptions :: ChirpOptions
startOptions = ChirpOptions
 { optStartFrequency = 0
 , optEndFrequency = 8000
 , optFrequencyShift = 0
 , optSampleRate = 44100
 , optRiseTime = 3
 , optRiseUnit = RiseUnitsSeconds
 , optInputSampleFormat = CL.SampleComplexDouble
 , optOutputSampleFormat = CL.SampleComplexDouble
 , optInputReader = CL.safeReader stdin
 , optOutputWriter = B.hPut stdout
 , optCloseInput = hClose stdin
 , optCloseOutput = hClose stdout
 , optSilenceLength = 44100
 , optSilenceTruncateLength = 0
 , optRepetitions = -1
 , optAmplitude = 0.5
 , optChirpWindow = NoWindow
 , optSignalWindow = NoWindow
 }


chirpOptionList :: [OptDescr (ChirpOptions -> IO ChirpOptions)]
chirpOptionList = 
    [ inputStartFrequency
    , inputEndFrequency
    , inputSampleRate
    , inputFrequencyShift
    , inputRiseTime
    , inputRiseSamples
    , inputOutputSignalFormat
    , inputSilenceLength
    , inputChirpWindow
    , inputFileOutput
    ]


commonMessage :: [String]
commonMessage = 
    [ "Chirp radar is a form of pulse compression radar. Instead of transmitting"
    , "a narrow pulse and listening for that reflection, chirp radar transmits a"
    , "chirp. A chirp is a frequency waveform that rises from a start frequency"
    , "to an end frequency over a set period governed by some sort of function."
    , "In the case of this program, the frequency increase is linear over time"
    , "making the chirp appear as a slope in the spectrogram. "
    , ""
    , "Upon receiving the chirp, the radar uses matched filtering to transform"
    , "that chirp into a narrow pulse. For best performance it is recommended"
    , "to keep the bandwidth-time product of the chirp above 100. Going"
    , "below 100 introduces ringing into the narrow pulse. This ringing degrades" 
    , "the ability of the radar to resolve targets."
    , ""
    , "Copyright Robert C. Taylor, 2017"
    , "This program is licensed under the Apache 2.0 license."
    , "The terms can be found at https://www.apache.org/licenses/LICENSE-2.0"
    , "You should also have received a NOTICE file along with this program."
    ]


chirpRadarRxOptions :: [OptDescr (ChirpOptions -> IO ChirpOptions)]
chirpRadarRxOptions = chirpOptionList ++ [
      inputSilenceTruncationLength
    , inputSignalWindow
    , inputFileInput
    , inputInputSignalFormat
    , optionHelp chirpRadarRxOptions (Just "ChirpRx")
    , optionAbout chirpRadarRxOptions (Just "ChirpRx") $ unlines $ ["", "This program processes a received chirp radar signal", ""] ++ commonMessage
    ]


chirpRadarTxOptions :: [OptDescr (ChirpOptions -> IO ChirpOptions)]
chirpRadarTxOptions = chirpOptionList ++ [
      inputRepetitions
    , inputAmplitude
    , optionHelp chirpRadarTxOptions (Just "ChirpTx")
    , optionAbout chirpRadarTxOptions (Just "ChirpTx") $ unlines $ ["", "This program transmits a chirp radar signal", ""] ++ commonMessage
    ]


optionAbout :: [OptDescr (ChirpOptions -> IO ChirpOptions)] -> Maybe String -> String -> OptDescr (ChirpOptions -> IO ChirpOptions)
optionAbout options mode extra = CL.inputAbout (CL.commonAboutHandler options mode extra)


optionHelp :: [OptDescr (ChirpOptions -> IO ChirpOptions)] -> Maybe String -> OptDescr (ChirpOptions -> IO ChirpOptions)
optionHelp options mode = CL.inputHelp (CL.commonHelpHandler options mode)


inputStartFrequency :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputStartFrequency = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Start frequency of the chirp"
          longOptionNames = ["startFrequency", "StartFrequency"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optStartFrequency = read input::Double }


inputEndFrequency :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputEndFrequency = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "End frequency of the chirp"
          longOptionNames = ["endFrequency", "EndFrequency"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optEndFrequency = read input::Double }


inputFrequencyShift :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputFrequencyShift = CL.inputFrequencyShift handler
    where handler input opts = return $ opts { optFrequencyShift = input }


inputSampleRate :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSampleRate = CL.inputSampleRate handler
    where handler input opts = return $ opts { optSampleRate = input }


inputRiseTime :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputRiseTime = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Rise time of the chirp"
          longOptionNames = ["riseTime", "RiseTime"]
          shortOptionsNames = []
          argExp = "seconds"
          handler input opts = return $ opts { optRiseTime = read input::Double, optRiseUnit = RiseUnitsSeconds }


inputRiseSamples :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputRiseSamples = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Rise samples of the chirp"
          longOptionNames = ["riseSamples", "RiseSamples"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optRiseTime = read input::Double, optRiseUnit = RiseUnitsSamples  }


inputInputSignalFormat :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputInputSignalFormat = CL.inputInputSignalFormat handler
    where handler input opts = return $ opts { optInputSampleFormat = input }


inputOutputSignalFormat :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputOutputSignalFormat = CL.inputOutputSignalFormat handler
    where handler input opts = return $ opts { optOutputSampleFormat = input }


inputSilenceLength :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSilenceLength = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Length of silence after chirp"
          longOptionNames = ["silenceLength", "SilenceLength"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optSilenceLength = read input::Int}


inputSilenceTruncationLength :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSilenceTruncationLength = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Reduce the length of a processing interval"
          longOptionNames = ["truncationLength", "TruncationLength"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optSilenceTruncateLength = read input::Int}


inputRepetitions :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputRepetitions = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Number of repetitions of the chirp"
          longOptionNames = ["repetitions", "Repetitions"]
          shortOptionsNames = []
          argExp = "count"
          handler input opts = return $ opts {optRepetitions = read input::Int}


inputAmplitude :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputAmplitude = CL.inputAmplitude handler
    where handler input opts = return $ opts {optAmplitude = input}


inputChirpWindow :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputChirpWindow = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Window to use on the chirp"
          longOptionNames = ["chirpWindow", "ChirpWindow"]
          shortOptionsNames = []
          argExp = "Hamming | None"
          handler input opts = return $ opts 
            { optChirpWindow = 
                case map DChar.toUpper input of
                     "HAMMING" -> HammingWindow
                     "NONE" -> NoWindow
                     _ -> error "Invalid window format"
            }


inputSignalWindow :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSignalWindow = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Window to use on the radar pulse"
          longOptionNames = ["pulseWindow", "PulseWindow"]
          shortOptionsNames = []
          argExp = "Hamming | None"
          handler input opts = return $ opts 
            { optSignalWindow = 
                case map DChar.toUpper input of
                     "HAMMING" -> HammingWindow
                     "NONE" -> NoWindow
                     _ -> error "Invalid window format"
            }


inputFileInput :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputFileInput = CL.inputFileInput handler
    where handler input opts = do
            (reader, closer) <- CL.commonInputFileHandler input
            return opts {optInputReader = reader, optCloseInput = closer}


inputFileOutput :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputFileOutput = CL.inputFileOutput handler
    where handler input opts = do
              (writer, closer) <- CL.commonOutputFileHandler input
              return opts { optOutputWriter  = writer, optCloseOutput = closer }




calculateSignalLength :: ChirpOptions -> Double
calculateSignalLength settings = case optRiseUnit settings of
                                      RiseUnitsSeconds -> rate * input
                                      RiseUnitsSamples -> input
    where input = optRiseTime settings
          rate = optSampleRate settings


getChirpWindow :: SignalWindow -> Int -> VUB.Vector Double
getChirpWindow window n = case window of
                          HammingWindow -> Windows.hammingWindowV n
                          NoWindow -> VUB.replicate n 1.0



 
