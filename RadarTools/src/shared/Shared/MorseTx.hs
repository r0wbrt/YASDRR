
module Shared.MorseTx where

import qualified Shared.CommandLine as CL
import qualified YASDRR.SDR.MorseCode as Morse
import qualified YASDRR.IO.ComplexSerialization as CS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as BP
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import Data.Complex


data MorseOptions = 
    MorseOptions { optionsInput :: IO String
                 , optionsSampleRate :: Double
                 , optionsWordsPerMinute :: Int
                 , optionsDotFrequency :: Double
                 , optionsOutputWriter :: BL.ByteString -> IO ()
                 , optionsOutputSignalFormat :: CL.SampleFormat
                 , optionsOutputCloser :: IO ()
                 , optionsAmplitude :: Double
                 }


-- | The default values of the options presented by this program. 
startOptions :: MorseOptions
startOptions =  
    MorseOptions { optionsInput = getContents
                 , optionsSampleRate = 44000::Double
                 , optionsWordsPerMinute = 20::Int
                 , optionsDotFrequency = 0::Double
                 , optionsOutputWriter = BL.hPut stdout
                 , optionsOutputSignalFormat = CL.SampleComplexDouble
                 , optionsOutputCloser = hClose stdout
                 , optionsAmplitude = 1.0
                 }

morseTxOptions :: [OptDescr (MorseOptions -> IO MorseOptions)]
morseTxOptions = 
    [ CL.inputFileInput (\input opt -> return opt { optionsInput = openFile input ReadMode >>= hGetContents  })
    , CL.inputMessage (\input opt -> return opt { optionsInput = (return input)})
    , CL.inputFileOutput inputFileOutput
    , CL.inputSampleRate (\input opt -> return opt { optionsSampleRate = input})
    , CL.inputFrequencyShift (\input opt -> return opt {optionsDotFrequency = input})
    , CL.inputAmplitude (\input opt -> return opt { optionsAmplitude = input})
    , CL.inputWpm (\input opt -> return opt {optionsWordsPerMinute = input})
    , CL.inputOutputSignalFormat (\input opt -> return opt {optionsOutputSignalFormat = input})
    , CL.inputAbout (CL.commonAboutHandler morseTxOptions (Just "MorseTx") descriptionMessage)
    , CL.inputHelp (CL.commonHelpHandler morseTxOptions (Just "MorseTx"))
    ]

inputFileOutput :: String -> MorseOptions -> IO MorseOptions
inputFileOutput input opt = do
    h <- openBinaryFile input WriteMode
    return opt {optionsOutputCloser = hClose h, optionsOutputWriter = BL.hPut h}

-- | Description message written to the command line describing how this program works.
descriptionMessage :: String
descriptionMessage = unlines message
    where message = [ ""
                    , "Input File (InputFile) A path to the text file that will be encoded into morse code."
                    , ""
                    , "Input Message (Message) A text message to encode into morse code."
                    , ""
                    , "Sample Rate (SampleRate) The sample rate of the generated morse code output."
                    , ""
                    , "Words Per Minute (WPM) The number of words to transmit per minute. "
                    , ""
                    , "Morse Frequency (Frequency) The center frequency of the generated morse signal."
                    , ""
                    , "Output Path (OutputPath) The file path to store the generated output."
                    , "Note, the output is stored as a complex floating point."
                    , ""
                    , "Set output to be a 11 bit complex integer. (SC11) When this flag is set," 
                    , "the output is instead a complex 16 bit sample with 11 bit precision."
                    , ""
                    , "The program expects Input File or Input Message to be defined. If neither is"
                    , "defined, the program will read from standard in."
                    , ""
                    , "The Sample Rate is in samples per second. It is advised that Sample Rate"
                    , "belong to the set of natural numbers. Fractional and negative sampling"
                    , "rates can be supplied to the program, however, the program's correct execution"
                    , "can not be guarenteed. Defaults to 44000hz."
                    , ""
                    , "Words Per Minute is defined according to the formula 1.2 / wpm. The output"
                    , "of this formula represents the duration of a dot in seconds. Defaults to 20 wpm."
                    , ""
                    , "Morse Frequency specifies the center frequency of the morse transmission."
                    , "This parameter defaults to 0 resulting in the morse transmission"
                    , "represented as a square wave."
                    , ""
                    , "Output Path defines where the encoded output of this program. If this is not"
                    , "defined, program will write to standard out."
                    , ""
                    , "SC11 flag results in the output of the program being encoded as a complex sc11"
                    , "signal. Here, an SC11 signal is a 16 bit integer with the first 11 bits of the"
                    , "number represeting values from (-1, 1). Used by some SDR HW platforms."
                    , ""
                    , ""
                    ]


processCommandInput :: GetOpt.ArgOrder (MorseOptions -> IO MorseOptions) -> [String] ->  (IO MorseOptions, [String], [String])
processCommandInput argOrder arguments = (CL.processInput startOptions actions, extra, errors)
    where (actions, extra, errors) = GetOpt.getOpt argOrder morseTxOptions arguments 


morseTxMainIO :: [String] -> IO ()
morseTxMainIO commandLineOptions = do
    case processCommandInput GetOpt.RequireOrder commandLineOptions of
              
         -- Normal Execution Path
         (parserResults, [], []) -> do
             
             executionSettings <- parserResults
             
             hSetBinaryMode stdout True 
             
             morseTxMain executionSettings
             
             hSetBinaryMode stdout False
             
             optionsOutputCloser executionSettings
             
          -- Case triggered when the user supplies invalid input
         (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure


morseTxMain :: MorseOptions -> IO ()
morseTxMain executionSettings = do
    textToEncode <- optionsInput executionSettings

    let morseSymbols = Morse.convertStringToMorseCode textToEncode
    
    let sampleRate = optionsSampleRate executionSettings
    
    let dotLength = Morse.wpmToDotLength (optionsWordsPerMinute executionSettings)
    
    let frequency = optionsDotFrequency executionSettings
    
    let outputFormat = case optionsOutputSignalFormat executionSettings of
                        CL.SampleComplexDouble -> CS.complexDoubleSerializer
                        CL.SampleComplexFloat  -> CS.complexFloatSerializer
                        CL.SampleComplexSigned16 -> CS.complexSigned16SerializerOne
    
    let signalWriter = optionsOutputWriter executionSettings
    
    let amplitude = optionsAmplitude executionSettings
    
    signalWriter $ BP.runPut $ generateMorseStream outputFormat sampleRate dotLength frequency amplitude morseSymbols


generateMorseStream :: (Complex Double -> BP.Put) -> Double -> Double -> Double -> Double -> ([Morse.MorseSymbol] -> BP.Put)
generateMorseStream serializer sampleRate dotLength frequency amplitude = mapM_ (generateMorseSymbol serializer sampleRate dotLength frequency amplitude)


generateMorseSymbol :: (Complex Double -> BP.Put) -> Double -> Double -> Double -> Double -> Morse.MorseSymbol -> BP.Put
generateMorseSymbol serializer sampleRate dotLength frequency amplitude symbol = mapM_ (\pos -> serializer $ generateMorseSample sampleRate frequency symAmplitude pos) [1 .. symLength]
    where symLength = floor $ sampleRate * dotLength * case symbol of
                                            Morse.MorseDot -> 1
                                            Morse.MorseDash -> 3
                                            Morse.MorseSpace -> 1
          symAmplitude = if symbol == Morse.MorseSpace then 0.0 else amplitude


generateMorseSample :: Double -> Double -> Double -> Int -> Complex Double
generateMorseSample sampleRate frequency amplitude pos = (amplitude :+ 0 ) * cis(2.0 * pi * frequency * fromIntegral pos / sampleRate)

