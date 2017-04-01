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
Module      :  Shared.CommandLine
Description :  Common code for processing the command line input into the program
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

-}

module Shared.CommandLine where

import System.Console.GetOpt as GetOpt
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Char as DChar
import System.Exit 
import System.IO


-- | Sample format field.
data SampleFormat = SampleComplexDouble | SampleComplexFloat | SampleComplexSigned16


--  | Mode of execution of the shared yasdrr program.
data ExecutionMode = ChirpReceive | ChirpTransmit | MorseTransmit | ExecutionModeNotSet | ExecutionModeInvalid


-- | Takes a list of functions and processes them into the final setting record.
--   Includes side effects.
processInput :: a -> [a -> IO a] -> IO a
processInput startOptions = foldl (>>=) (return startOptions) 


-- | Handles the --about option.
inputAbout :: (a -> IO a) -> OptDescr (a -> IO a)
inputAbout handler  = GetOpt.Option shortOptionsNames longOptionNames (GetOpt.NoArg handler) description 
    where description = "Show about message"
          longOptionNames = ["about", "About"]
          shortOptionsNames = []
          

-- | Common about handler that can be used by other modules to display an about message
--   to the end user.
commonAboutHandler :: [OptDescr (a -> IO a)] -> Maybe String -> String  -> a -> IO a
commonAboutHandler options mode extraInfo _ = do
    prg <- getProgName
    hPutStrLn stderr $ GetOpt.usageInfo ("Usage: "++prg++""++flag++" [OPTIONS...]") options ++ extraInfo
    exitSuccess
    where flag = case mode of
                    Just exMode -> " --mode="++exMode++" "
                    Nothing -> ""
              

-- | Handles the help input option.
inputHelp :: (a -> IO a) -> OptDescr (a -> IO a)
inputHelp handler = GetOpt.Option shortOptionsNames longOptionNames (GetOpt.NoArg handler) description 
    where description = "Show this help message"
          longOptionNames = ["help", "Help"]
          shortOptionsNames = ['h']


-- | Common help function that can be used by other modules to display help messages
--   to the end user.
commonHelpHandler :: [OptDescr (a -> IO a)] -> Maybe String -> a -> IO a
commonHelpHandler options mode _ = do
              prg <- getProgName
              hPutStrLn stderr (GetOpt.usageInfo ("Usage: "++prg++""++flag++" [OPTIONS...]") options) 
              exitSuccess
    where flag = case mode of
                    Just exMode -> " --mode="++exMode++" "
                    Nothing -> ""


-- | Handles the input mode command line flag.
inputMode :: (ExecutionMode -> a -> IO a) -> OptDescr (a -> IO a)
inputMode recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Mode of operation"
          longOptionNames = ["mode", "Mode"]
          shortOptionsNames = []
          argExp = "ChirpRx | ChirpTx | MorseTx"
          handler input = recordHandler (getModeFromString input)


-- | Process the input string into the actual mode of execution
getModeFromString :: String -> ExecutionMode
getModeFromString input = case map DChar.toUpper input of
                                                "CHIRPRX" -> ChirpReceive
                                                "CHIRPTX" -> ChirpTransmit
                                                "MORSETX" -> MorseTransmit
                                                _ -> ExecutionModeInvalid


-- | Processes the amplitude command line flag into its actual value.
inputAmplitude :: (Double -> a -> IO a) ->  OptDescr (a -> IO a)
inputAmplitude recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Amplitude of the chirp"
          longOptionNames = ["amplitude", "Amplitude"]
          shortOptionsNames = []
          argExp = "amplitude"
          handler input = recordHandler (read input::Double)


-- | Handles the file output option.
inputFileOutput :: (String -> a -> IO a) -> OptDescr (a -> IO a)
inputFileOutput handler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "File to write output to"
          longOptionNames = ["output", "Output"]
          shortOptionsNames = []
          argExp = "FILE"


-- | Reusable function, which can be used by other modules, to handle opening files based on 
--   command line input.
commonOutputFileHandler :: String -> IO (B.ByteString -> IO (), IO ()) 
commonOutputFileHandler input = do
    handle <- openBinaryFile input WriteMode
    return (B.hPut handle, hClose handle)


-- | Handles the command line option that sets the signal output format
inputOutputSignalFormat :: (SampleFormat -> a -> IO a) -> OptDescr (a -> IO a)
inputOutputSignalFormat = inputSignalFormatHandler description ["signalOutputFormat", "SignalOutputFormat"]
    where description = "Format of output signal"


-- | Handles the command line option that sets the signals input format.
inputInputSignalFormat :: (SampleFormat -> a -> IO a) -> OptDescr (a -> IO a)
inputInputSignalFormat = inputSignalFormatHandler description ["signalInputFormat", "SignalInputFormat"]
    where description = "Format of input signal"


-- | Handles the command line option that sets the input signal format.
inputSignalFormatHandler :: String -> [String] -> (SampleFormat -> a -> IO a) -> OptDescr (a -> IO a)
inputSignalFormatHandler description longOptionNames recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where shortOptionsNames = []
          argExp = "Double | Float | Signed16"
          handler input = recordHandler $ getSampleFormatFromString input


-- | Command line option that sets the input file source.
inputFileInput :: (String -> a -> IO a) -> OptDescr (a -> IO a)
inputFileInput handler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "File to read input from"
          longOptionNames = ["input", "Input"]
          shortOptionsNames = []
          argExp = "FILE"


-- | Common function that takes a path and returns a reader function and 
--   closer function.
commonInputFileHandler :: String -> IO (Int -> IO B.ByteString, IO ())
commonInputFileHandler input = do
    handle <- openBinaryFile input ReadMode
    return (safeReader handle, hClose handle)


-- | A reader that avoids throwing an EOF exception
safeReader :: Handle -> Int -> IO B.ByteString
safeReader h size = do
    result <- hIsEOF h
    if result then
        return B.empty
    else
        B.hGet h size


-- | Converts a string specifying the sample format into the actual 
--   SampleFormat type.
getSampleFormatFromString :: String -> SampleFormat
getSampleFormatFromString input = case map DChar.toUpper input of
                                                "DOUBLE" -> SampleComplexDouble
                                                "FLOAT" -> SampleComplexFloat
                                                "SIGNED16" -> SampleComplexSigned16
                                                _ -> error "Invalid signal format"


-- | Handles the command line option that specifies the sample rate.
inputSampleRate :: (Double -> a -> IO a) -> OptDescr (a -> IO a)
inputSampleRate recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Sample rate of signal"
          longOptionNames = ["sampleRate", "SampleRate"]
          shortOptionsNames = []
          argExp = "frequency * samples * s^-1"
          handler input = recordHandler (read input::Double)


-- | Handles the command line option specifying the frequency shift.
inputFrequencyShift :: (Double -> a -> IO a) -> OptDescr (a -> IO a)
inputFrequencyShift recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Frequency shift of the signal"
          longOptionNames = ["frequencyShift", "FrequencyShift"]
          shortOptionsNames = []
          argExp = "frequency * hz * s^-1"
          handler input = recordHandler (read input::Double)


-- | Handles the command line option that supplies a text message.
inputMessage :: (String -> a -> IO a) -> OptDescr (a -> IO a)
inputMessage handler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "A text message to encode into morse code"
          longOptionNames = ["inputMessage", "InputMessage"]
          shortOptionsNames = []
          argExp = "Input Message to convert"


-- | Handles the command line option that specifies the wpm
inputWpm :: (Int -> a -> IO a) -> OptDescr (a -> IO a)
inputWpm recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "The number of words to transmit per minute"
          longOptionNames = ["wpm", "WPM"]
          shortOptionsNames = []
          argExp = "Words * minutes^-1"
          handler input = recordHandler (read input::Int)


-- | Displays a list of errors and then quits the program.
programInputError :: [String] -> IO ()
{-# ANN module "HLint: ignore Use :" #-}
programInputError errors = do
    hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
    exitFailure


-- | Validates the record using a list of validators
validateOptions :: a -> [a -> [String] -> [String]] -> [String]
validateOptions options = foldl (\l r -> r options l) []
