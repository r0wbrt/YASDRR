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
Module      :  Shared.ChirpCommon
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

-- Non yasdrr libraries
import System.Console.GetOpt as GetOpt
import System.IO
import qualified Data.Char as DChar
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VUB


--yasdrr libraries
import YASDRR.SDR.ChirpRadar (SignalWindow (..) )
import qualified YASDRR.DSP.Windows as Windows


--yasrr executable modules
import qualified Shared.CommandLine as CL


-- | Units of the rise time input from the command line.
data RiseUnits = RiseUnitsSeconds | RiseUnitsSamples


-- | Record representing the options of the yasdrr executable.
data ChirpOptions = ChirpOptions 
 { 
   -- The start frequency that the chirp "rises" from. May be negative or positive.
   
   optStartFrequency :: Double
   
   -- The end frequency of the chirp. Can be greater then or less then the start 
   -- frequency since chirps can both rise and fall.
   
 , optEndFrequency :: Double
 
   -- Constant frequency offset of the generated signal.
   
 , optFrequencyShift :: Double
 
   -- The sample rate of the chirp.
   
 , optSampleRate :: Double
 
   -- The amount of time the chirp will take to go from start to end. Note, the 
   -- units of this number are stored in optRiseUnit.
 
 , optRiseTime :: Double
 
   -- The actual units of rise time. May be samples or seconds.
 
 , optRiseUnit :: RiseUnits
 
   -- The input sample format of the signal. Used by the chirp receiver.
 
 , optInputSampleFormat :: CL.SampleFormat
 
   -- The output format of the generated signal. Used by both receiver and transmitter.
 
 , optOutputSampleFormat :: CL.SampleFormat
 
   -- The function that reads the input into the program when doing chirp
   -- receive processing.
 
 , optInputReader :: Int -> IO B.ByteString
 
   -- The function that writes the output of the program.
 
 , optOutputWriter :: B.ByteString -> IO ()
 
   -- The amount of silence between chirp pulses.
 
 , optSilenceLength :: Int
 
   -- The amount of samples to truncate from a signal pulse.
 
 , optSilenceTruncateLength :: Int
 
   -- The number of times to send a transmitted pulse.
   
 , optRepetitions :: Int
 
   -- The amplitude of the chirp.
   
 , optAmplitude :: Double
 
   -- Window to use on the generated chirp.
   
 , optChirpWindow :: SignalWindow
 
   -- Window to use on the received signal
 
 , optSignalWindow :: SignalWindow
 
   -- function to close the input
 
 , optCloseInput :: IO ()
 
  -- function to close the output of the program
 
 , optCloseOutput :: IO ()
 } 


-- | The default start options of the chirp programs.
startOptions :: ChirpOptions
startOptions = ChirpOptions
 { optStartFrequency = 0
 , optEndFrequency = 8000
 , optFrequencyShift = 0
 , optSampleRate = 44100 -- https://en.wikipedia.org/wiki/44,100_Hz
 , optRiseTime = 3
 , optRiseUnit = RiseUnitsSeconds
 , optInputSampleFormat = CL.SampleComplexDouble
 , optOutputSampleFormat = CL.SampleComplexDouble
 , optInputReader = CL.safeReader stdin
 , optOutputWriter = B.hPut stdout
 , optCloseInput = hClose stdin
 , optCloseOutput = hClose stdout
 , optSilenceLength = 44100 ---1 second at the default sample rate.
 , optSilenceTruncateLength = 0
 , optRepetitions = 1 -- generate one pulse.
 , optAmplitude = 0.2 -- Don't overload equipment by default
 , optChirpWindow = NoWindow
 , optSignalWindow = NoWindow
 }


-- | List of common chirp command line options.
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


-- | Message explaining the chirp programs to the command line user.
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


-- | Actual command line options for the chirp rx mode.
chirpRadarRxOptions :: [OptDescr (ChirpOptions -> IO ChirpOptions)]
chirpRadarRxOptions = chirpOptionList ++ [
      inputSilenceTruncationLength
    , inputSignalWindow
    , inputFileInput
    , inputInputSignalFormat
    , optionHelp chirpRadarRxOptions (Just "ChirpRx")
    , optionAbout chirpRadarRxOptions (Just "ChirpRx") $ unlines $ ["", "This program processes a received chirp radar signal", ""] ++ commonMessage
    ]


-- | Actual command line options for the chirp tx mode.
chirpRadarTxOptions :: [OptDescr (ChirpOptions -> IO ChirpOptions)]
chirpRadarTxOptions = chirpOptionList ++ [
      inputRepetitions
    , inputAmplitude
    , optionHelp chirpRadarTxOptions (Just "ChirpTx")
    , optionAbout chirpRadarTxOptions (Just "ChirpTx") $ unlines $ ["", "This program transmits a chirp radar signal", ""] ++ commonMessage
    ]


-- | list of validators for the RX chirp mode.
chirpRadarRxValidators ::  [ChirpOptions -> [String] -> [String]]
chirpRadarRxValidators = 
    [ validateSampleRate
    , validateRiseTime
    , validateSilenceLength
    , validateSilenceTruncationLength
    ]


-- | List of validators for the TX chirp mode.
chirpRadarTxValidators ::  [ChirpOptions -> [String] -> [String]]
chirpRadarTxValidators = 
    [ validateSampleRate
    , validateRiseTime
    , validateSilenceLength
    , validateRepetitions
    ]

-- | --about option command line handler.
optionAbout :: [OptDescr (ChirpOptions -> IO ChirpOptions)] -> Maybe String -> String -> OptDescr (ChirpOptions -> IO ChirpOptions)
optionAbout options mode extra = CL.inputAbout (CL.commonAboutHandler options mode extra)


-- | --help option command line handler.
optionHelp :: [OptDescr (ChirpOptions -> IO ChirpOptions)] -> Maybe String -> OptDescr (ChirpOptions -> IO ChirpOptions)
optionHelp options mode = CL.inputHelp (CL.commonHelpHandler options mode)


-- | --startFrequency command line option handler.
inputStartFrequency :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputStartFrequency = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Start frequency of the chirp"
          longOptionNames = ["startFrequency", "StartFrequency"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optStartFrequency = read input::Double }


-- | --endFrequency command line option handler
inputEndFrequency :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputEndFrequency = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "End frequency of the chirp"
          longOptionNames = ["endFrequency", "EndFrequency"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optEndFrequency = read input::Double }


-- | comamnd line handler for the frequency shift option of the signal
inputFrequencyShift :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputFrequencyShift = CL.inputFrequencyShift handler
    where handler input opts = return $ opts { optFrequencyShift = input }


-- | command line handler which sets the sample rate of the program.
inputSampleRate :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSampleRate = CL.inputSampleRate handler
    where handler input opts = return $ opts { optSampleRate = input }


-- | command line handler for setting the rise time of the chirp
inputRiseTime :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputRiseTime = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Rise time of the chirp"
          longOptionNames = ["riseTime", "RiseTime"]
          shortOptionsNames = []
          argExp = "seconds"
          handler input opts = return $ opts { optRiseTime = read input::Double, optRiseUnit = RiseUnitsSeconds }


-- | command line handler for setting the rise samples of the chirp.
inputRiseSamples :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputRiseSamples = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Rise samples of the chirp"
          longOptionNames = ["riseSamples", "RiseSamples"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optRiseTime = read input::Double, optRiseUnit = RiseUnitsSamples  }


-- | command line handler for setting the input signal format of the signal.
inputInputSignalFormat :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputInputSignalFormat = CL.inputInputSignalFormat handler
    where handler input opts = return $ opts { optInputSampleFormat = input }


-- | command line handler for setting the output signal format.
inputOutputSignalFormat :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputOutputSignalFormat = CL.inputOutputSignalFormat handler
    where handler input opts = return $ opts { optOutputSampleFormat = input }


-- | Sets the length of silence following a chirp based on the command line input.
inputSilenceLength :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSilenceLength = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Length of silence after chirp"
          longOptionNames = ["silenceLength", "SilenceLength"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optSilenceLength = read input::Int}


-- | Sets the amount of samples to slice off a signal based on user input
inputSilenceTruncationLength :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputSilenceTruncationLength = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Reduce the length of a processing interval"
          longOptionNames = ["truncationLength", "TruncationLength"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optSilenceTruncateLength = read input::Int}


-- | Sets the number of pulses to transmit using the command line input.
inputRepetitions :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputRepetitions = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Number of repetitions of the chirp"
          longOptionNames = ["repetitions", "Repetitions"]
          shortOptionsNames = []
          argExp = "count"
          handler input opts = return $ opts {optRepetitions = read input::Int}


-- | Sets the amplitude of the chirp.
inputAmplitude :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputAmplitude = CL.inputAmplitude handler
    where handler input opts = return $ opts {optAmplitude = input}


-- | Sets the window to apply to the input signal before chirp processing.
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


-- | Sets the input signal window to use on the received radar wave form
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


-- | Opens the input file to read the chirp signals from.
inputFileInput :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputFileInput = CL.inputFileInput handler
    where handler input opts = do
            (reader, closer) <- CL.commonInputFileHandler input
            return opts {optInputReader = reader, optCloseInput = closer}


-- | Sets the output file to read the chirp signals into.
inputFileOutput :: OptDescr (ChirpOptions -> IO ChirpOptions)
inputFileOutput = CL.inputFileOutput handler
    where handler input opts = do
              (writer, closer) <- CL.commonOutputFileHandler input
              return opts { optOutputWriter  = writer, optCloseOutput = closer }


-- | Calculates the length of the signal in samples based on the 
--   the unit of the input rise time.
calculateSignalLength :: ChirpOptions -> Double
calculateSignalLength settings = case optRiseUnit settings of
                                      RiseUnitsSeconds -> rate * input
                                      RiseUnitsSamples -> input
    where input = optRiseTime settings
          rate = optSampleRate settings


-- | Generates the signal window.
getChirpWindow :: SignalWindow -> Int -> VUB.Vector Double
getChirpWindow window n = case window of
                          HammingWindow -> Windows.hammingWindowV n
                          NoWindow -> VUB.replicate n 1.0


-- | Validates the sample rate of the signal.
validateSampleRate :: ChirpOptions -> [String] -> [String]
validateSampleRate options errors = if optSampleRate options <= 0 then  errorMessage:errors else errors
    where errorMessage = "Sample rate must be greater then 0."


-- | Validates rise time
validateRiseTime :: ChirpOptions -> [String] -> [String]
validateRiseTime options errors = if optRiseTime options <= 0 then errorMessage:errors else errors
    where errorMessage = "Rise time must be greater then 0."


-- | Validates silence length
validateSilenceLength :: ChirpOptions -> [String] -> [String]
validateSilenceLength options errors = if optSilenceLength options < 0 then errorMessage:errors else errors
    where errorMessage = "Silence length may not be less then 0."


-- | validate silence truncation length
validateSilenceTruncationLength :: ChirpOptions -> [String] -> [String]
validateSilenceTruncationLength options errors = if optSilenceTruncateLength options < 0 then errorMessage:errors else errors
    where errorMessage = "Silence truncation length must not be less then 0."

-- | validates the repetition option.
validateRepetitions :: ChirpOptions -> [String] -> [String]
validateRepetitions options errors = if optRepetitions options /= -1 && optRepetitions options <=0 then errorMessage:errors else errors
    where errorMessage = "Repetitions must be greater then 0 or -1 to indicate infinite repetition."





 
