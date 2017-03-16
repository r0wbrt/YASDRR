--Copyright Robert C. Taylor, All Rights Reserved

-- This program will write its output in a binary mode.
import qualified Data.ByteString as B

-- This library is used to parse the command line options passed into this program.
import System.Console.GetOpt as GetOpt

--Used to read text files that will be encoded into a morse signal.
import System.IO

--Functions to encode text and generate the resulting morse code.
import qualified YASDRR.SDR.MorseCode as Morse

--Needed to import environment variables like the command line and program name.
import qualified System.Environment as Environment

--Functions used to terminate the program.
import System.Exit 

--Functions to convert complex signals into their binary form.
import qualified YASDRR.IO.ComplexSerialization as ComplexSerialization


data ProgramOptions = ProgramOptions { optionsInput :: IO String
                                        , optionsSampleRate :: Double
                                        , optionsWordsPerMinute :: Int
                                        , optionsDotFrequency :: Double
                                        , optionsOutputWriter :: B.ByteString -> IO ()
                                        , optionsEncodeAsSC11 :: Bool
                                        , optionsOutputCloser :: IO ()
                                        }

                                        
-- | The default values of the options presented by this program. 
defaulProgramOptions :: ProgramOptions
defaulProgramOptions =  ProgramOptions { optionsInput = getContents
                                       , optionsSampleRate = 44000::Double
                                       , optionsWordsPerMinute = 20::Int
                                       , optionsDotFrequency = 0::Double
                                       , optionsOutputWriter = B.hPut stdout
                                       , optionsEncodeAsSC11 = False
                                       , optionsOutputCloser = hClose stdout
                                       }

                                       
-- | Definitions describing acceptable command line input into this program and
--   how to decode said input.
commandLineOptions :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
commandLineOptions = 
    [ GetOpt.Option [] ["inputFile"] (GetOpt.ReqArg (\input options -> return options { optionsInput = openFile input ReadMode >>= hGetContents  }) "[Path to Text File]")
        "A path to the text file that will be encoded into morse code."
        
    , GetOpt.Option [] ["inputMessage"] (GetOpt.ReqArg (\input options -> return options { optionsInput = return input }) "[Input Message to convert]")  
        "A text message to encode into morse code."
        
    , GetOpt.Option [] ["sampleRate"] (GetOpt.ReqArg (\input options -> return options { optionsSampleRate = read input::Double }) "[Sample Rate of the morse signal]")
        "The sample rate of the generated morse code output."
        
    , GetOpt.Option [] ["wpm"] (GetOpt.ReqArg (\input options -> return options { optionsWordsPerMinute = read input::Int }) "[The number of words to transmit per minute]")
        "The number of words to transmit per minute."
        
    , GetOpt.Option [] ["frequency"] (GetOpt.ReqArg (\input options -> return options { optionsDotFrequency = read input::Double }) "[Center frequency of the morse signal]")
        "The center frequency of the generated morse signal."
    
    , GetOpt.Option [] ["outputPath"] (GetOpt.ReqArg (\input options -> do
                                         h <- openBinaryFile input WriteMode 
                                         return options { optionsOutputWriter = B.hPut h, optionsOutputCloser = hClose h}) "[Center frequency of the morse signal]")
        "The file path to store the generated output. Note, the output is stored as a complex floating point."

    , GetOpt.Option [] ["sc11"] (GetOpt.NoArg (\options -> return options { optionsEncodeAsSC11 = True }) )
        "When this flag is set, the output is instead a complex 16 bit sample with 11 bit precision."
        
     , GetOpt.Option ['h', '?'] ["help"] (GetOpt.NoArg (showHelpMessage False)) "Show simple help message"
     
     , GetOpt.Option [] ["about"] (GetOpt.NoArg (showHelpMessage True)) "Show extended help message"
    ]
 
 
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


-- | Shows the programs help message and then terminates the program.
showHelpMessage :: Bool -> ProgramOptions -> IO ProgramOptions
showHelpMessage showAll _ = do
    programName <- Environment.getProgName
    let extendedDescription = if showAll then descriptionMessage else ""
    hPutStrLn stderr (GetOpt.usageInfo (extendedDescription ++ unlines ["", "Usage: "++programName++" [OPTIONS...]"])  commandLineOptions) 
    exitSuccess
    
-- | Writes a single morse symbol to the output file
writeMorseSymbol:: ([a] -> B.ByteString) -> ([Morse.MorseSymbol] -> [a]) -> (B.ByteString -> IO ()) -> Morse.MorseSymbol -> IO ()
writeMorseSymbol serializer morseGenerator writer symbol = writer $ serializer $ morseGenerator [symbol]
    
-- | The programs main execution function.
{-# ANN module "HLint: ignore Use :" #-}
main :: IO ()
main = do
    commandLineArguments <- Environment.getArgs 
    case GetOpt.getOpt GetOpt.RequireOrder commandLineOptions commandLineArguments of
              
         -- Normal Execution Path
         (parserResults, [], []) -> do
             
             executionSettings <- foldl (>>=) (return defaulProgramOptions) parserResults
             
             hSetBinaryMode stdout True 
             
             textToEncode <- optionsInput executionSettings
             
             let morseSymbols = Morse.convertStringToMorseCode textToEncode
             
             let sampleRate = optionsSampleRate executionSettings
             
             let dotLength = Morse.wpmToDotLength (optionsWordsPerMinute executionSettings)
             
             let frequency = optionsDotFrequency executionSettings
             
             let morseSignalGenerator = Morse.generateMorseCodeFromSequence sampleRate frequency (1.0::Double) dotLength
             
             let binarySignalEncoder = if optionsEncodeAsSC11 executionSettings then
                                            ComplexSerialization.serializeBlock ComplexSerialization.complexSC11Serializer
                                                else ComplexSerialization.serializeBlock ComplexSerialization.complexFloatSerializer
                                                
             let writer = optionsOutputWriter executionSettings
                 
             _ <- mapM (writeMorseSymbol binarySignalEncoder morseSignalGenerator writer) morseSymbols
                
             hSetBinaryMode stdout False
             
             optionsOutputCloser executionSettings
             
             
          -- Case triggered when the user supplies invalid input
         (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure


        
