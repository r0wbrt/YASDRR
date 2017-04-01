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
Module      :  Shared.MorseTx
Description :  Code implementing a morse code transmitter.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This module has functionality to generate morse code waveforms at arbitary 
sample rates. The code has been tested up to 26MSPS. However, the program
here is far too slow for realtime use at 26MSPS based on tests performed
on a 3.4Ghz i7-6700 CPU. However, the program was deemed fast enough to run at 
its target rate, 1.5MSPS. The program generated 20 seconds of 20wpm morse code 
in under 4 seconds.

-}

module Shared.MorseTx where

import qualified Shared.CommandLine as CL
import qualified YASDRR.SDR.MorseCode as Morse
import qualified Shared.IO as SIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as BP
import System.Console.GetOpt as GetOpt
import System.IO
import Data.Complex
import qualified Control.Monad as CM


-- | Settings controling the morse transmitter.
data MorseOptions = 
    MorseOptions { 
                   -- Input to encode into a morse transmission
                   
                   optionsInput :: IO String  
                   
                   -- The sample rate of the morse transmission
                   
                 , optionsSampleRate :: Double 
                 
                   -- The keying rate of the morse transmission
                   
                 , optionsWordsPerMinute :: Int 
                 
                   -- Offset of the generated wave form from zero Hz
                   
                 , optionsDotFrequency :: Double 
                 
                   -- IO path to route the output waveform. Generally a 
                   -- function that writes to a file. However, it could 
                   -- hypothetically just be some sort of message channel,
                   -- so code should not assume this goes to a file.
                   
                 , optionsOutputWriter :: BL.ByteString -> IO () 
                 
                   -- Format of the transmitted signal
                   
                 , optionsOutputSignalFormat :: CL.SampleFormat 
                 
                   -- Function to call that will perform any final actions 
                   -- on the output stream.
                   
                 , optionsOutputCloser :: IO () 
                 
                   -- Amplitude of the morse signal.
                   
                 , optionsAmplitude :: Double 
                 }


-- | The default values of the options. By default, the morse input string is 
--   read from stdin and the generated output written to stdout.
startOptions :: MorseOptions
startOptions =  
    MorseOptions { optionsInput = getContents
                 , optionsSampleRate = 44100::Double -- https://en.wikipedia.org/wiki/44,100_Hz
                 , optionsWordsPerMinute = 20::Int
                 , optionsDotFrequency = 600::Double
                 , optionsOutputWriter = BL.hPut stdout
                 , optionsOutputSignalFormat = CL.SampleComplexDouble
                 , optionsOutputCloser = hClose stdout
                 , optionsAmplitude = 0.2 -- Don't overload equipment by default.
                 }


-- | Functions to process the command line input and modify startOptions.
morseTxOptions :: [OptDescr (MorseOptions -> IO MorseOptions)]
morseTxOptions = 
    [ CL.inputFileInput (\input opt -> return opt { optionsInput = openFile input ReadMode >>= hGetContents  })
    , CL.inputMessage (\input opt -> return opt { optionsInput = return input})
    , CL.inputFileOutput inputFileOutput
    , CL.inputSampleRate (\input opt -> return opt { optionsSampleRate = input})
    , CL.inputFrequencyShift (\input opt -> return opt {optionsDotFrequency = input})
    , CL.inputAmplitude (\input opt -> return opt { optionsAmplitude = input})
    , CL.inputWpm (\input opt -> return opt {optionsWordsPerMinute = input})
    , CL.inputOutputSignalFormat (\input opt -> return opt {optionsOutputSignalFormat = input})
    , CL.inputAbout (CL.commonAboutHandler morseTxOptions (Just "MorseTx") descriptionMessage)
    , CL.inputHelp (CL.commonHelpHandler morseTxOptions (Just "MorseTx"))
    ]


-- | Ensures the supplied words per minute makes sense.
validateWpm :: MorseOptions -> [String] -> [String]
validateWpm options list = if wpm < 1 then errorMessage:list else list
    where wpm = optionsWordsPerMinute options
          errorMessage = "WPM must be greater then or equal to 1."


-- | Ensures the supplied sampling rate makes sense.
validateSampleRate :: MorseOptions -> [String] -> [String]
validateSampleRate options list = if sampleRate <= 0 then errorMessage:list else list
    where sampleRate = optionsSampleRate options 
          errorMessage = "Sampling rate must be greater then zero."


-- | CommandLine handler that will open a output stream to the supplied file path.
--   The file will be written to using a lazy byte string.
inputFileOutput :: String -> MorseOptions -> IO MorseOptions
inputFileOutput input opt = do
    h <- openBinaryFile input WriteMode
    return opt {optionsOutputCloser = hClose h, optionsOutputWriter = BL.hPut h}


-- | Description message written to the command line describing how this program works.
descriptionMessage :: String

-- If possible, it would be nice to move this text to a resource file for I18N
-- reasons.
descriptionMessage = unlines message
    where message = [ "" 
                    , "Given an ascii string, this program generates the associated morse code"
                    , "signal."
                    , ""
                    , "Morse code is an international standard for transmitting text over the air"
                    , "via simple On-Off keying where the length of each symbol represents either"
                    , "a dot or a dash. Patterns of these dots and dash represents letters, numbers,"
                    , "and spaces."
                    , ""
                    , ""
                    , "Unless specified, the following settings are used:"
                    , ""
                    , "* If not input or output is specified then this program will read and write"
                    , "  to standard in and standard out."
                    , ""
                    , "* The amplitude of the signal is 0.2."
                    , ""
                    , "* The sampling rate of the output signal is 44100."
                    , ""
                    , "* Words per minute is 20."
                    , ""
                    , "* The dot frequency is 600hz."
                    , ""
                    , ""
                    , "Note: The length of a single dot is calculated from words per minute by"
                    , "      dividing 1.2 by the wpm (1.2 / [wpm])."
                    , ""
                    , ""
                    , "Copyright 2017 Robert C. Taylor"
                    , "Licensed under the Apache License, Version 2.0 (the \"License\");"
                    , "You may obtain a copy of the License at"
                    , ""
                    , "http://www.apache.org/licenses/LICENSE-2.0"
                    , ""
                    , "Unless required by applicable law or agreed to in writing, software"
                    , "distributed under the License is distributed on an \"AS IS\" BASIS,"
                    , "WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."
                    , "See the License for the specific language governing permissions and"
                    , "limitations under the License."
                    ]


-- | Takes a list of strings representing command line input and processes them 
--   into the MorseOptions record.
processCommandInput :: GetOpt.ArgOrder (MorseOptions -> IO MorseOptions) -> [String] ->  (IO MorseOptions, [String], [String])
processCommandInput argOrder arguments = (CL.processInput startOptions actions, extra, errors)
    where (actions, extra, errors) = GetOpt.getOpt argOrder morseTxOptions arguments 


-- | Standalone version of the morse program. Called by yasdrr when mode is set
--   to MorseTx. Closes output handle and exits the program if it encounters
--   invalid command line options.
morseTxMainIO :: [String] -> IO ()
morseTxMainIO commandLineOptions = 
    case processCommandInput GetOpt.RequireOrder commandLineOptions of
              
         -- Normal Execution Path
         (parserResults, [], []) -> do
             
             executionSettings <- parserResults
             
             let errorCheck = CL.validateOptions executionSettings [validateWpm, validateSampleRate]
             
             CM.when (errorCheck /= []) (CL.programInputError errorCheck)
             
             hSetBinaryMode stdout True 
             
             morseTxMain executionSettings
             
             hSetBinaryMode stdout False
             
             optionsOutputCloser executionSettings
             
          -- Case triggered when the user supplies invalid input
         (_, _, errors) -> CL.programInputError errors


-- | Encodes a input string into a morse code signal and returns the output
--   to the supplied outputWriter. Does not close streams upon termination.
--   Performs no bound checking on supplied settings.
morseTxMain :: MorseOptions -> IO ()
morseTxMain executionSettings = do
    textToEncode <- optionsInput executionSettings

    let morseSymbols = Morse.convertStringToMorseCode textToEncode
    
    let sampleRate = optionsSampleRate executionSettings
    
    let dotLength = Morse.wpmToDotLength (optionsWordsPerMinute executionSettings)
    
    let frequency = optionsDotFrequency executionSettings
    
    let outputFormat = case optionsOutputSignalFormat executionSettings of
    
                        CL.SampleComplexDouble -> SIO.complexDoubleSerializer
                        
                        CL.SampleComplexFloat  -> SIO.complexFloatSerializer
                        
                        -- Using a serializer wiht no division, even when that 
                        -- division is one, so theoretically, the compilier should
                        -- have optimized the division out, had better performance.
                        CL.SampleComplexSigned16 -> SIO.complexSigned16SerializerOne
    
    let signalWriter = optionsOutputWriter executionSettings
    
    let amplitude = optionsAmplitude executionSettings
    
    -- Performance testing sugested that putting the algorithm that generates 
    -- the raw signal into this same file along with its output as a lazy byte 
    -- string produced the best performance both in time and space complexity.
    
    signalWriter $ BP.runPut $ generateMorseStream outputFormat sampleRate dotLength frequency amplitude morseSymbols


-- | Takes a list of morse symbols and translates them into a complex stream.
generateMorseStream :: (Complex Double -> BP.Put) -> Double -> Double ->
                            Double -> Double -> ([Morse.MorseSymbol] -> BP.Put)
generateMorseStream serializer sampleRate dotLength frequency amplitude = mapM_ (generateMorseSymbol serializer sampleRate dotLength frequency amplitude)


-- | Takes a morse symbol and generates its corresponding signal using the supplied
--   input parameters.
generateMorseSymbol :: (Complex Double -> BP.Put) -> Double -> Double 
                            -> Double -> Double -> Morse.MorseSymbol -> BP.Put
generateMorseSymbol serializer sampleRate dotLength frequency amplitude symbol = mapM_ (serializer . generateMorseSample sampleRate frequency symAmplitude) [1 .. symLength]
    
    -- It seems wierd to have symbol lengths that are not integer in length. 
    -- It could be done with interpolation, but that functionality does not
    -- exist here. So the code is written to floor the symbol length to an 
    -- integer.
    where symLength = floor $ sampleRate * dotLength * case symbol of
                                            Morse.MorseDot -> 1
                                            Morse.MorseDash -> 3
                                            Morse.MorseSpace -> 1
          symAmplitude = if symbol == Morse.MorseSpace then 0.0 else amplitude


-- | Generates a single complex morse sample.
generateMorseSample :: Double -> Double -> Double -> Int -> Complex Double
generateMorseSample sampleRate frequency amplitude pos = (amplitude :+ 0 ) * cis(2.0 * pi * frequency * fromIntegral pos / sampleRate)
