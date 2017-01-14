--Copyright Robert C. Taylor, All Rights Reserved

import Data.Complex
import Data.Typeable
import qualified OFDMRadar.Threading.Sharding as RMS
import qualified Data.ByteString as B
import Control.Exception
import System.Environment
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import Data.List    
import Data.List.Split       
import Data.List
import qualified Data.Vector as V
import OFDMRadar.IO.ComplexSerialization
import OFDMRadar.SDR.OFDMModulation


--Program options record
data ProgramOptions = ProgramOptions { optionsOutputWriter :: B.ByteString -> IO ()
                                        , optionsCloseOutputWriter :: IO ()
                                        , optionsInputGetter   ::  Int -> IO B.ByteString
                                        , optionsImpulseGetter ::  Int -> IO B.ByteString
                                        , optionsInputSize :: Int
                                        , optionsImpulseSize :: Int
                                        , optionsDopplerSize :: Int
                                        , optionsInputCyclicShifts :: [Double]
                                        , optionsSCQ11Input :: Bool
                                        }
                
                
--Default arguments
startOptions = ProgramOptions   { optionsOutputWriter = B.hPut stdout
                                , optionsCloseOutputWriter = hClose stdout
                                , optionsInputGetter = B.hGet stdin
                                , optionsImpulseGetter = B.hGet stdin
                                , optionsInputSize = 512
                                , optionsImpulseSize = 16
                                , optionsDopplerSize = 1
                                , optionsInputCyclicShifts = [0]
                                , optionsSCQ11Input = False
                                }
                
                
--Program Options
options :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
options = 
    [ GetOpt.Option ['i'] ["inputStream"] (GetOpt.ReqArg (\path opt -> do
                                                                handle <- openBinaryFile path ReadMode
                                                                return opt {optionsInputGetter = (safeReader handle)})
        "[Signal Input Path]" ) "The signal stream to process for radar returns"
    , GetOpt.Option ['o'] ["outputStream"] (GetOpt.ReqArg (\path opt -> do
                                                                handle <- openBinaryFile path WriteMode
                                                                return opt {optionsOutputWriter = (B.hPut handle), optionsCloseOutputWriter = (hClose handle)}) "[Signal Output Path]" ) "The path to write the radar processed output to. Output is double i/q format."
    , GetOpt.Option ['y'] ["symbolStream"] (GetOpt.ReqArg (\path opt -> do
                                                                handle <- openBinaryFile path ReadMode
                                                                return opt {optionsImpulseGetter = (safeReader handle)}) "[Symbol Input Path]" ) "The path to read the last symbol transmitted by the radar from"
    , GetOpt.Option ['s'] ["symbolSize"] (GetOpt.ReqArg (\size opt -> return opt { optionsImpulseSize = read size::Int } )  "[Symbol Size]" )  "The size in samples of a radar symbol"
    , GetOpt.Option ['d'] ["dwellInterval"] (GetOpt.ReqArg (\interval opt -> return opt { optionsInputSize = read interval::Int } ) "[Dwell Time]")  "The number of samples to process for each pulse"
    , GetOpt.Option ['h', '?'] ["help", "about"] (GetOpt.NoArg (\_ -> do
                                                                prg <- getProgName
                                                                hPutStrLn stderr (GetOpt.usageInfo ("Usage: "++prg++" [OPTIONS...]") options) 
                                                                exitWith ExitSuccess)) "Show this help message"
    , GetOpt.Option ['p'] ["dopplerInterval"] (GetOpt.ReqArg (\interval opt -> return opt { optionsDopplerSize = read interval::Int } ) "[Number of Pulses]")  "The number of pulses to perform doppler processing over"
    , GetOpt.Option [] ["cyclicShifts"] (GetOpt.ReqArg (\string opt -> return opt { optionsInputCyclicShifts = processCyclicShifts string } ) "[List of cyclic Shifts]") "Cyclic shifts to perform upon the input signal. \r\nComma delimited list of numbers or number expansion. Ex 1,2,3, which is equivalent to 1:3:1.\r\nCan combine numbers with expansion operations, eg \"-2:2:.25,-5,5,-10,10\""
    , GetOpt.Option [] ["scQ11Input"]
        (GetOpt.NoArg (\opt -> return $ opt { optionsSCQ11Input = True })) "Optionally makes signalInput SCQ11 encoded for 12 bit ADC"
    ]
    
    
--Avoids throwing an EOF exception
safeReader handle size = do
    result <- hIsEOF handle
    if result then
        return B.empty
    else
        B.hGet handle size
    
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder options arguments of
          (actions, [], []) -> do
              options <- foldl (>>=) (return startOptions) actions
              
              --Set 0 & 1 to binary since this is where the program reads and writes by default.
              hSetBinaryMode stdin True 
              hSetBinaryMode stdout True 
              
              let inputByteSize = if optionsSCQ11Input options then 4 else 8
              
              let inputReader = (optionsInputGetter options) $ ((optionsInputSize options) * inputByteSize) * (optionsDopplerSize options)
              let impulseReader = (optionsImpulseGetter options) ((optionsImpulseSize options) * 8) 
              
              let workerThread = radarWorkerThread (fromIntegral (optionsInputSize options)) (optionsInputCyclicShifts options) (optionsSCQ11Input options)
              let readerThread = readerWorkerThread (inputReader) (impulseReader) ((fromIntegral ((optionsInputSize options) * (optionsDopplerSize options))) * inputByteSize) ((optionsImpulseSize options) * 8)
              let writerThread = writerWorkerThread (optionsOutputWriter options)
              
              --Begin thread execution
              shardHandle <- RMS.shardResource readerThread [] writerThread [] workerThread []
             
              --Wait for workers to terminate
              RMS.waitForCompletion shardHandle
              
              --Close output function to force a flush of all buffered output data to the OS.
              optionsCloseOutputWriter options
              exitWith ExitSuccess
          (_, _, errors) -> do
              (hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors)
              exitFailure
              
              
processComplexRadarReturn shifts impulse pulses = V.map (\shift -> processOfdmRadarReturnV impulse shift pulses) (V.fromList shifts)

 
--Parse the cyclic shift input string
processCyclicShifts string = sort $ singleOptions ++ expansionOptions -- return opt { optionsInputCyclicShifts = sort $ singleOptions ++ expansionOptions}
    where stringSettingList = map (splitOn ":") $ splitOn "," string
          singleOptions = map (\[input] -> read input::Double) $ filter (\l -> length l == 1) stringSettingList
          expansionOptions = concat $ map listComprehender $ filter (\l -> length l == 3) stringSettingList
          listComprehender :: [String] -> [Double]
          listComprehender (x:y:z:xs) = [(i * step) + start | i <- [0..((end-start) / step)]]
            where start = read x::Double
                  end = read y::Double
                  step = read z::Double
                  

encodeSignalBlock signalBlock = serializeBlock complexFloatSerializer $ (V.toList $ V.concatMap (id) $ V.concatMap (id) signalBlock)
               
               
--Each worker thread will encode and decode signal blocks in addition to performing radar processing on them.
radarWorkerThread signalSize shifts sc11Input state (impulseBlock, inSignalBlock) = do
    
    --
    let decoder = if sc11Input then complexSC11Deserializer else complexFloatDeserializer
    
    --Decode the signal impulse 
    let impulse = V.fromList $ fst $ deserializeBlock complexFloatDeserializer impulseBlock
    
    --Decode the recieved signal
    let signal = V.fromList $ map (V.fromList) $ fst $ deserializeBlock (blockListDeserializer decoder signalSize) inSignalBlock
    
    --Process the signal and the impulse
    let results = processComplexRadarReturn shifts impulse signal
    
    --Encode the results of the processing
    let outSignalBlock = encodeSignalBlock results
    
    return (Just $ (outSignalBlock, []))
    
    
readerWorkerThread :: IO B.ByteString -> IO B.ByteString -> Int -> Int -> [Char] -> IO (Maybe ((B.ByteString, B.ByteString), [Char]))
readerWorkerThread inputReader impulseReader inputSize impulseSize state = do
    
        impulseBlock <- impulseReader
        inputBlock <- inputReader
        
        let check = ((B.length impulseBlock) /= impulseSize) || ((B.length inputBlock) /= inputSize)
        
        return $ if check then
        
            Nothing 
            
                 else Just $ ((impulseBlock, inputBlock), [])

                 
writerWorkerThread :: (B.ByteString -> IO ()) -> [Char] -> B.ByteString -> IO (Maybe [Char])
writerWorkerThread outputWriter state outSignalBlock = do
    
    outputWriter outSignalBlock
    
    return (Just $ [])
     


