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

module Options where

import System.Environment
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import Data.Complex
import qualified Data.Char as DChar
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified YASDRR.DSP.Windows as Windows
import qualified YASDRR.IO.ComplexSerialization as IOComplex

data RiseUnits = RiseUnitsSeconds | RiseUnitsSamples


data SampleFormat = SampleComplexDouble | SampleComplexFloat | SampleComplexSigned16


data SignalWindow = HammingWindow | NoWindow


data ProgramOptions = ProgramOptions 
 { optStartFrequency :: Double
 , optEndFrequency :: Double
 , optFrequencyShift :: Double
 , optSampleRate :: Double
 , optRiseTime :: Double
 , optRiseUnit :: RiseUnits
 , optInputSampleFormat :: SampleFormat
 , optOutputSampleFormat :: SampleFormat
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
 
 
startOptions :: ProgramOptions
startOptions = ProgramOptions
 { optStartFrequency = 0
 , optEndFrequency = 8000
 , optFrequencyShift = 0
 , optSampleRate = 44100
 , optRiseTime = 3
 , optRiseUnit = RiseUnitsSeconds
 , optInputSampleFormat = SampleComplexDouble
 , optOutputSampleFormat = SampleComplexDouble
 , optInputReader = safeReader stdin
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
 
 
chirpOptionList :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
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
    , "and makes the chirp appear as a slope in the spectrogram. "
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
    
 
chirpRadarRxOptions :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
chirpRadarRxOptions = chirpOptionList ++ [
      inputSilenceTruncationLength
    , inputSignalWindow
    , inputFileInput
    , inputInputSignalFormat
    , optionHelp chirpRadarRxOptions
    , optionAbout chirpRadarRxOptions $ unlines $ ["", "This program processes a received chirp radar signal", ""] ++ commonMessage
    ]
    
    
chirpRadarTxOptions :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
chirpRadarTxOptions = chirpOptionList ++ [
      inputRepetitions
    , inputAmplitude
    , optionHelp chirpRadarTxOptions
    , optionAbout chirpRadarTxOptions $ unlines $ ["", "This program transmits a chirp radar signal", ""] ++ commonMessage
    ]
    
    
processInput :: [ProgramOptions -> IO ProgramOptions] -> IO ProgramOptions
processInput = foldl (>>=) (return startOptions)

optionAbout :: [OptDescr (ProgramOptions -> IO ProgramOptions)] -> String -> OptDescr (ProgramOptions -> IO ProgramOptions)
optionAbout options extraInfo  = GetOpt.Option shortOptionsNames longOptionNames (GetOpt.NoArg handler) description 
    where description = "Show about message"
          longOptionNames = ["about", "About"]
          shortOptionsNames = []
          handler _ = do
              prg <- getProgName
              hPutStrLn stderr $ (GetOpt.usageInfo ("Usage: "++prg++" [OPTIONS...]") options) ++ extraInfo
              exitSuccess
              
              
optionHelp :: [OptDescr (ProgramOptions -> IO ProgramOptions)] -> OptDescr (ProgramOptions -> IO ProgramOptions)
optionHelp options = GetOpt.Option shortOptionsNames longOptionNames (GetOpt.NoArg handler) description 
    where description = "Show this help message"
          longOptionNames = ["help", "Help"]
          shortOptionsNames = ['h']
          handler _ = do
              prg <- getProgName
              hPutStrLn stderr (GetOpt.usageInfo ("Usage: "++prg++" [OPTIONS...]") options) 
              exitSuccess
              
              
inputStartFrequency :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputStartFrequency = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Start frequency of the chirp"
          longOptionNames = ["startFrequency", "StartFrequency"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optStartFrequency = read input::Double }
          
          
inputEndFrequency :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputEndFrequency = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "End frequency of the chirp"
          longOptionNames = ["endFrequency", "EndFrequency"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optEndFrequency = read input::Double }
          
          
inputFrequencyShift :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputFrequencyShift = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Frequency shift of the signal"
          longOptionNames = ["frequencyShift", "FrequencyShift"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input opts = return $ opts { optFrequencyShift = read input::Double }
          
          
inputSampleRate :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputSampleRate = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Sample rate of signal"
          longOptionNames = ["sampleRate", "SampleRate"]
          shortOptionsNames = []
          argExp = "frequency * samples * s^-1"
          handler input opts = return $ opts { optSampleRate = read input::Double }
          
          
inputRiseTime :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputRiseTime = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Rise time of the chirp"
          longOptionNames = ["riseTime", "RiseTime"]
          shortOptionsNames = []
          argExp = "seconds"
          handler input opts = return $ opts { optRiseTime = read input::Double, optRiseUnit = RiseUnitsSeconds }
          
          
inputRiseSamples :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputRiseSamples = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Rise samples of the chirp"
          longOptionNames = ["riseSamples", "RiseSamples"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optRiseTime = read input::Double, optRiseUnit = RiseUnitsSamples  }
          
          
inputInputSignalFormat :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputInputSignalFormat = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Format of input signal"
          longOptionNames = ["signalInputFormat", "SignalInputFormat"]
          shortOptionsNames = []
          argExp = "Double | Float | Signed16"
          handler input opts = return $ opts 
            { optInputSampleFormat = 
                case map (DChar.toUpper) input of
                     "DOUBLE" -> SampleComplexDouble
                     "FLOAT" -> SampleComplexFloat
                     "SIGNED16" -> SampleComplexSigned16
                     _ -> error "Invalid signal format"
            }
            
            
inputOutputSignalFormat :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputOutputSignalFormat = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Format of output signal"
          longOptionNames = ["signalOutputFormat", "SignalOutputFormat"]
          shortOptionsNames = []
          argExp = "Double | Float | Signed16"
          handler input opts = return $ opts 
            { optOutputSampleFormat = 
                case map (DChar.toUpper) input of
                     "DOUBLE" -> SampleComplexDouble
                     "FLOAT" -> SampleComplexFloat
                     "SIGNED16" -> SampleComplexSigned16
                     _ -> error "Invalid signal format"
            }
            
            
inputSilenceLength :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputSilenceLength = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Length of silence after chirp"
          longOptionNames = ["silenceLength", "SilenceLength"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optSilenceLength = read input::Int}
          
          
inputSilenceTruncationLength :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputSilenceTruncationLength = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Reduce the length of a processing interval"
          longOptionNames = ["truncationLength", "TruncationLength"]
          shortOptionsNames = []
          argExp = "samples"
          handler input opts = return $ opts {optSilenceTruncateLength = read input::Int}
          
          
inputRepetitions :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputRepetitions = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Number of repetitions of the chirp"
          longOptionNames = ["repetitions", "Repetitions"]
          shortOptionsNames = []
          argExp = "count"
          handler input opts = return $ opts {optRepetitions = read input::Int}
          
          
inputAmplitude :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputAmplitude = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Amplitude of the chirp"
          longOptionNames = ["amplitude", "Amplitude"]
          shortOptionsNames = []
          argExp = "amplitude"
          handler input opts = return $ opts {optAmplitude = read input::Double}
          
          
inputChirpWindow :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputChirpWindow = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Window to use on the chirp"
          longOptionNames = ["chirpWindow", "ChirpWindow"]
          shortOptionsNames = []
          argExp = "Hamming | None"
          handler input opts = return $ opts 
            { optChirpWindow = 
                case map (DChar.toUpper) input of
                     "HAMMING" -> HammingWindow
                     "NONE" -> NoWindow
                     _ -> error "Invalid window format"
            }
            
            
inputSignalWindow :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputSignalWindow = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Window to use on the radar pulse"
          longOptionNames = ["pulseWindow", "PulseWindow"]
          shortOptionsNames = []
          argExp = "Hamming | None"
          handler input opts = return $ opts 
            { optSignalWindow = 
                case map (DChar.toUpper) input of
                     "HAMMING" -> HammingWindow
                     "NONE" -> NoWindow
                     _ -> error "Invalid window format"
            }
            
            
inputFileInput :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputFileInput = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "File to read input from"
          longOptionNames = ["input", "Input"]
          shortOptionsNames = []
          argExp = "FILE"
          handler input opts = do
              handle <- openBinaryFile input ReadMode
              return opts { optInputReader  = safeReader handle, optCloseInput = hClose handle }
              
              
inputFileOutput :: OptDescr (ProgramOptions -> IO ProgramOptions)
inputFileOutput = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "File to write output to"
          longOptionNames = ["output", "Output"]
          shortOptionsNames = []
          argExp = "FILE"
          handler input opts = do
              handle <- openBinaryFile input WriteMode
              return opts { optOutputWriter  = B.hPut handle, optCloseOutput = hClose handle }
              
              
--Avoids throwing an EOF exception
safeReader :: Handle -> Int -> IO B.ByteString
safeReader h size = do
    result <- hIsEOF h
    if result then
        return B.empty
    else
        B.hGet h size
        
        
calculateSignalLength :: ProgramOptions -> Double
calculateSignalLength settings = case optRiseUnit settings of
                                      RiseUnitsSeconds -> rate * input
                                      RiseUnitsSamples -> input
    where input = optRiseTime settings
          rate = optSampleRate settings
          
          
getChirpWindow :: SignalWindow -> Int -> V.Vector (Double)
getChirpWindow window n = case window of
                          HammingWindow -> Windows.hammingWindowV n
                          NoWindow -> V.replicate n 1.0
                          
                          
serializeOutput :: SampleFormat -> V.Vector (Complex Double) -> B.ByteString
serializeOutput format signal = case format of
                                   SampleComplexDouble -> IOComplex.serializeBlockV IOComplex.complexDoubleSerializer signal
                                   SampleComplexFloat -> IOComplex.serializeBlockV IOComplex.complexFloatSerializer signal
                                   SampleComplexSigned16 -> IOComplex.serializeBlockV (IOComplex.complexSigned16Serializer 1.0) signal
