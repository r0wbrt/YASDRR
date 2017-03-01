--Copyright Robert C. Taylor, All Rights Reserved

import Data.Complex
import qualified YASDRR.Threading.Sharding as RMS
import qualified Data.ByteString as B
import System.Environment
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import Data.List    
import Data.List.Split       
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VUB
import YASDRR.IO.ComplexSerialization
import YASDRR.SDR.OFDMModulation


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
startOptions :: ProgramOptions
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
programOptions :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
programOptions = 
    [ GetOpt.Option ['i'] ["inputStream"] (GetOpt.ReqArg (\path opt -> do
                                                                h <- openBinaryFile path ReadMode
                                                                return opt {optionsInputGetter = safeReader h})
        "[Signal Input Path]" ) "The signal stream to process for radar returns"
    , GetOpt.Option ['o'] ["outputStream"] (GetOpt.ReqArg (\path opt -> do
                                                                h <- openBinaryFile path WriteMode
                                                                return opt {optionsOutputWriter = B.hPut h, optionsCloseOutputWriter = hClose h}) "[Signal Output Path]" ) "The path to write the radar processed output to. Output is double i/q format."
    , GetOpt.Option ['y'] ["symbolStream"] (GetOpt.ReqArg (\path opt -> do
                                                                h <- openBinaryFile path ReadMode
                                                                return opt {optionsImpulseGetter = safeReader h}) "[Symbol Input Path]" ) "The path to read the last symbol transmitted by the radar from"
    , GetOpt.Option ['s'] ["symbolSize"] (GetOpt.ReqArg (\size opt -> return opt { optionsImpulseSize = read size::Int } )  "[Symbol Size]" )  "The size in samples of a radar symbol"
    , GetOpt.Option ['d'] ["dwellInterval"] (GetOpt.ReqArg (\interval opt -> return opt { optionsInputSize = read interval::Int } ) "[Dwell Time]")  "The number of samples to process for each pulse"
    , GetOpt.Option ['h', '?'] ["help", "about"] (GetOpt.NoArg (\_ -> do
                                                                prg <- getProgName
                                                                hPutStrLn stderr (GetOpt.usageInfo ("Usage: "++prg++" [OPTIONS...]") programOptions) 
                                                                exitSuccess)) "Show this help message"
    , GetOpt.Option ['p'] ["dopplerInterval"] (GetOpt.ReqArg (\interval opt -> return opt { optionsDopplerSize = read interval::Int } ) "[Number of Pulses]")  "The number of pulses to perform doppler processing over"
    , GetOpt.Option [] ["cyclicShifts"] (GetOpt.ReqArg (\string opt -> return opt { optionsInputCyclicShifts = processCyclicShifts string } ) "[List of cyclic Shifts]") "Cyclic shifts to perform upon the input signal. \r\nComma delimited list of numbers or number expansion. Ex 1,2,3, which is equivalent to 1:3:1.\r\nCan combine numbers with expansion operations, eg \"-2:2:.25,-5,5,-10,10\""
    , GetOpt.Option [] ["sc11"]
        (GetOpt.NoArg (\opt -> return $ opt { optionsSCQ11Input = True })) "Optionally makes signalInput SCQ11 encoded for 12 bit ADC"
    ]
    
    
--Avoids throwing an EOF exception
safeReader :: Handle -> Int -> IO B.ByteString
safeReader h size = do
    result <- hIsEOF h
    if result then
        return B.empty
    else
        B.hGet h size

{-# ANN module "HLint: ignore Use :" #-}
main :: IO ()
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder programOptions arguments of
          (actions, [], []) -> do
              options <- foldl (>>=) (return startOptions) actions
              
              --Set 0 & 1 to binary since this is where the program reads and writes by default.
              hSetBinaryMode stdin True 
              hSetBinaryMode stdout True 
              
              let inputByteSize = if optionsSCQ11Input options then 4 else 8
              
              let inputReader = optionsInputGetter options $ (optionsInputSize options * inputByteSize) * optionsDopplerSize options
              let impulseReader = optionsImpulseGetter options (optionsImpulseSize options * 8) 
              
              let workerThread = radarWorkerThread (fromIntegral (optionsInputSize options)) (optionsInputCyclicShifts options) (optionsSCQ11Input options)
              let readerThread = readerWorkerThread inputReader impulseReader (fromIntegral (optionsInputSize options * optionsDopplerSize options) * inputByteSize) (optionsImpulseSize options * 8)
              let writerThread = writerWorkerThread (optionsOutputWriter options)
              
              --Begin thread execution
              shardHandle <- RMS.shardResource readerThread [] writerThread [] workerThread []
             
              --Wait for workers to terminate
              RMS.waitForCompletion shardHandle
              
              --Close output function to force a flush of all buffered output data to the OS.
              optionsCloseOutputWriter options
              exitSuccess
          (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure
              

processComplexRadarReturn :: [Double] -> VUB.Vector (Complex Double) ->
                                VB.Vector (VUB.Vector (Complex Double)) ->
                                VB.Vector (VB.Vector (VUB.Vector (Complex Double)))
processComplexRadarReturn shifts impulse pulses = VB.map (\shift -> processOfdmRadarReturnV impulse shift pulses) (VB.fromList shifts)

 
--Parse the cyclic shift input string
processCyclicShifts :: String -> [Double]
processCyclicShifts string = sort $ singleOptions ++ expansionOptions -- return opt { optionsInputCyclicShifts = sort $ singleOptions ++ expansionOptions}
    where stringSettingList = map (splitOn ":") $ splitOn "," string
          singleOptions = map (\[input] -> read input::Double) $ filter (\l -> length l == 1) stringSettingList
          expansionOptions = concatMap listComprehender $ filter (\l -> length l == 3) stringSettingList
          listComprehender :: [String] -> [Double]
          listComprehender (x:y:z:_) = [(i * step) + start | i <- [0..((end-start) / step)]]
            where start = read x::Double
                  end = read y::Double
                  step = read z::Double
          listComprehender _ = error "Invalid cyclic shift option format"
         
                  

encodeSignalBlock :: VB.Vector (VB.Vector (VUB.Vector (Complex Double))) ->
                            B.ByteString 
encodeSignalBlock signalBlock = serializeBlock complexFloatSerializer $ VB.toList (VB.concatMap VUB.convert $ VB.concatMap id signalBlock)
               
               
--Each worker thread will encode and decode signal blocks in addition to 
--performing radar processing on them.
radarWorkerThread :: Int -> [Double] -> Bool -> [Int] -> (B.ByteString, B.ByteString)
                                -> IO (Maybe (B.ByteString, [Int]))
radarWorkerThread signalSize shifts sc11Input _ (impulseBlock, inSignalBlock) = do
    
    --
    let decoder = if sc11Input then complexSC11Deserializer else complexFloatDeserializer
    
    --Decode the signal impulse 
    let impulse = VUB.fromList $ fst $ deserializeBlock complexFloatDeserializer impulseBlock
    
    --Decode the recieved signal
    let signal = VB.fromList $ map VUB.fromList $ fst $ deserializeBlock (blockListDeserializer decoder signalSize) inSignalBlock
    
    --Process the signal and the impulse
    let results = processComplexRadarReturn shifts impulse signal
    
    --Encode the results of the processing
    let outSignalBlock = encodeSignalBlock results
    
    return (Just (outSignalBlock, []))
    
    
readerWorkerThread :: IO B.ByteString -> IO B.ByteString -> Int -> Int -> String -> IO (Maybe ((B.ByteString, B.ByteString), String))
readerWorkerThread inputReader impulseReader inputSize impulseSize _ = do
    
        impulseBlock <- impulseReader
        inputBlock <- inputReader
        
        let check = (B.length impulseBlock /= impulseSize) || (B.length inputBlock /= inputSize)
        
        return $ if check then
        
            Nothing 
            
                 else Just ((impulseBlock, inputBlock), [])

                 
writerWorkerThread :: (B.ByteString -> IO ()) -> String -> B.ByteString -> 
                        IO (Maybe String)
writerWorkerThread outputWriter _ outSignalBlock = do
    
    outputWriter outSignalBlock
    
    return (Just [])
     


