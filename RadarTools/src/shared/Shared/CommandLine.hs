

module Shared.CommandLine where

import System.Console.GetOpt as GetOpt
import System.Environment
import qualified Data.ByteString as B
import qualified Data.Char as DChar
import System.Exit 
import System.IO

data SampleFormat = SampleComplexDouble | SampleComplexFloat | SampleComplexSigned16

data ExecutionMode = ChirpReceive | ChirpTransmit | MorseTransmit | ExecutionModeNotSet | ExecutionModeInvalid


processInput :: a -> [a -> IO a] -> IO a
processInput startOptions = foldl (>>=) (return startOptions) 


inputAbout :: (a -> IO a) -> OptDescr (a -> IO a)
inputAbout handler  = GetOpt.Option shortOptionsNames longOptionNames (GetOpt.NoArg handler) description 
    where description = "Show about message"
          longOptionNames = ["about", "About"]
          shortOptionsNames = []
          

commonAboutHandler :: [OptDescr (a -> IO a)] -> Maybe String -> String  -> a -> IO a
commonAboutHandler options mode extraInfo _ = do
    prg <- getProgName
    hPutStrLn stderr $ GetOpt.usageInfo ("Usage: "++prg++""++flag++" [OPTIONS...]") options ++ extraInfo
    exitSuccess
    where flag = case mode of
                    Just exMode -> " --mode="++exMode++" "
                    Nothing -> ""
              

inputHelp :: (a -> IO a) -> OptDescr (a -> IO a)
inputHelp handler = GetOpt.Option shortOptionsNames longOptionNames (GetOpt.NoArg handler) description 
    where description = "Show this help message"
          longOptionNames = ["help", "Help"]
          shortOptionsNames = ['h']


commonHelpHandler :: [OptDescr (a -> IO a)] -> Maybe String -> a -> IO a
commonHelpHandler options mode _ = do
              prg <- getProgName
              hPutStrLn stderr (GetOpt.usageInfo ("Usage: "++prg++""++flag++" [OPTIONS...]") options) 
              exitSuccess
    where flag = case mode of
                    Just exMode -> " --mode="++exMode++" "
                    Nothing -> ""

inputMode :: (ExecutionMode -> a -> IO a) -> OptDescr (a -> IO a)
inputMode recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Mode of operation"
          longOptionNames = ["mode", "Mode"]
          shortOptionsNames = []
          argExp = "ChirpRx | ChirpTx | MorseTx"
          handler input = recordHandler (getModeFromString input)


getModeFromString :: String -> ExecutionMode
getModeFromString input = case map DChar.toUpper input of
                                                "CHIRPRX" -> ChirpReceive
                                                "CHIRPTX" -> ChirpTransmit
                                                "MORSETX" -> MorseTransmit
                                                _ -> ExecutionModeInvalid


inputAmplitude :: (Double -> a -> IO a) ->  OptDescr (a -> IO a)
inputAmplitude recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Amplitude of the chirp"
          longOptionNames = ["amplitude", "Amplitude"]
          shortOptionsNames = []
          argExp = "amplitude"
          handler input = recordHandler (read input::Double)


inputFileOutput :: (String -> a -> IO a) -> OptDescr (a -> IO a)
inputFileOutput handler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "File to write output to"
          longOptionNames = ["output", "Output"]
          shortOptionsNames = []
          argExp = "FILE"


commonOutputFileHandler :: String -> IO (B.ByteString -> IO (), IO ()) 
commonOutputFileHandler input = do
    handle <- openBinaryFile input WriteMode
    return $ (B.hPut handle, hClose handle)


inputOutputSignalFormat :: (SampleFormat -> a -> IO a) -> OptDescr (a -> IO a)
inputOutputSignalFormat recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Format of output signal"
          longOptionNames = ["signalOutputFormat", "SignalOutputFormat"]
          shortOptionsNames = []
          argExp = "Double | Float | Signed16"
          handler input = recordHandler $ getSampleFormatFromString input


inputInputSignalFormat :: (SampleFormat -> a -> IO a) -> OptDescr (a -> IO a)
inputInputSignalFormat recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Format of input signal"
          longOptionNames = ["signalInputFormat", "SignalInputFormat"]
          shortOptionsNames = []
          argExp = "Double | Float | Signed16"
          handler input = recordHandler $ getSampleFormatFromString input


getSampleFormatFromString :: String -> SampleFormat
getSampleFormatFromString input = case map DChar.toUpper input of
                                                "DOUBLE" -> SampleComplexDouble
                                                "FLOAT" -> SampleComplexFloat
                                                "SIGNED16" -> SampleComplexSigned16
                                                _ -> error "Invalid signal format"


inputSampleRate :: (Double -> a -> IO a) -> OptDescr (a -> IO a)
inputSampleRate recordHandler = GetOpt.Option shortOptionsNames longOptionNames (ReqArg handler argExp) description 
    where description = "Sample rate of signal"
          longOptionNames = ["sampleRate", "SampleRate"]
          shortOptionsNames = []
          argExp = "frequency * samples * s^-1"
          handler input = recordHandler (read input::Double)

