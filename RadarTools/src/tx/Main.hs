--Copyright Robert C. Taylor, All Rights Reserved

module Main where

import qualified Data.ByteString as B
import Data.Complex
import System.Environment
import System.Exit
import qualified OFDMRadar.Threading.Sharding as RMS
import System.Console.GetOpt as GetOpt
import System.IO
import qualified Data.Array as Array
import Data.Word
import OFDMRadar.Math.Misc
import Data.List.Split
import System.Random
import OFDMRadar.IO.ComplexSerialization
import qualified OFDMRadar.SDR.OFDMModulation as SdrOFDMMod

--Default number of carriers to generate. 
defaultNumberOfCarriers :: Int
defaultNumberOfCarriers = 32

--Data structure where all the options will post-processing of commandline input.
data ProgramOptions = ProgramOptions { optionsTransmitOutputWriter  :: B.ByteString -> IO ()
                                        , optionsCloseTransmitOutputWriter :: IO ()
                                        , optionsSymbolOutputWriter :: B.ByteString -> IO ()
                                        , optionsCloseSymbolOutputWriter :: IO ()
                                        , optionsDataInputReader :: Int -> IO B.ByteString
                                        , optionsCloseDataInputReader :: IO ()
                                        , optionsNumberOfCarriers :: Int
                                        , optionsCyclicPrefixLength :: Int
                                        , optionsLengthOfSilence :: Int
                                        , optionsConstellationPoints :: [(Word8, Complex Double)]
                                        , optionsSCQ11Output :: Bool
                                        , optionsRandomData :: Bool
                                        , optionsNumberOfFrames :: Int
                                        }
        
startOptions :: ProgramOptions
startOptions = ProgramOptions { optionsTransmitOutputWriter = B.hPut stdout
                                , optionsCloseTransmitOutputWriter = hClose stdout
                                , optionsSymbolOutputWriter = B.hPut stdout
                                , optionsCloseSymbolOutputWriter = hClose stdout
                                , optionsDataInputReader = safeReader stdin
                                , optionsCloseDataInputReader = hClose stdin
                                , optionsCyclicPrefixLength = 0
                                , optionsLengthOfSilence = 0
                                
                                --Number of carriers equal number of default number of carriers
                                , optionsNumberOfCarriers = defaultNumberOfCarriers
                                --Scale the BPSK signal to be normalized with power of 1.0
                                , optionsConstellationPoints = [(0, (1.0 / fromIntegral defaultNumberOfCarriers) :+ 0), (1, (-1.0 / fromIntegral defaultNumberOfCarriers) :+ 0)]
                                
                                , optionsSCQ11Output = False
                                , optionsRandomData = False
                                , optionsNumberOfFrames = -1
                                }

                                
--The actual program options that describe how program options will
--be populated and what the options do.
options :: [OptDescr (ProgramOptions -> IO ProgramOptions)]
options = 
    [ GetOpt.Option ['o'] ["signalOutput"] 
        (GetOpt.ReqArg (\path opt -> do
                                        handle <- openBinaryFile path WriteMode
                                        return $ opt { optionsTransmitOutputWriter = B.hPut handle, optionsCloseTransmitOutputWriter = hClose handle}) "output path") "The output path of the raw signal data in double I/Q format"
    , GetOpt.Option ['y'] ["symbolOutput"]
        (GetOpt.ReqArg (\path opt -> do
                                        handle <- openBinaryFile path WriteMode
                                        return $ opt { optionsSymbolOutputWriter = B.hPut handle, optionsCloseSymbolOutputWriter = hClose handle }) "symbol output path") "The output path of the raw symbol, used in radar correlation processing"
    , GetOpt.Option ['i'] ["dataInput"]
        (GetOpt.ReqArg (\path opt -> do
                                        handle <- openBinaryFile path ReadMode
                                        return $ opt { optionsDataInputReader = safeReader handle, optionsCloseDataInputReader = hClose handle }) "data to encode path") "The input path to encode data"
    , GetOpt.Option ['s'] ["symbolSize"]
        (GetOpt.ReqArg (\iString opt -> return $ opt { optionsNumberOfCarriers = read iString::Int }) "Size of the symbol") "Size in samples of a single symbol. Must be a power of 2. The size also represents the number of carriers in a single symbol."
    , GetOpt.Option ['p'] ["cyclicPrefixSize"] 
        (GetOpt.ReqArg (\iString opt -> return $ opt { optionsCyclicPrefixLength = read iString::Int }) "Size of cyclic prefix") "Size in number of samples of the cyclic prefix"
    , GetOpt.Option ['d'] ["deadTime"]
        (GetOpt.ReqArg (\iString opt -> return $ opt {optionsLengthOfSilence = read iString::Int}) "Number of dead samples to insert between transmissions") "Size in number of samples to transmit no values between successive symbols"
    , GetOpt.Option ['c'] ["constellation"]
    (GetOpt.ReqArg (\iString opt -> return $ opt { optionsConstellationPoints = programOptionsConstellationComprehender iString}) "constellation") "Constellation to encode data with. Format constellation size in 2^n followed by ordered 3-tuples seperated by spaces, eg QPSK \"2 (0,1,0) (1,0,1) (2,-1,0) (3,0,-1)\""
     , GetOpt.Option ['h', '?'] ["help", "about"] (GetOpt.NoArg (\_ -> do
                                                                prg <- getProgName
                                                                hPutStrLn stderr (GetOpt.usageInfo ("Usage: "++prg++" [OPTIONS...]") options) 
                                                                exitSuccess)) "Show this help message"
     , GetOpt.Option [] ["scQ11Output"]
        (GetOpt.NoArg (\opt -> return $ opt { optionsSCQ11Output = True })) "Optionally makes signalOutput SC Q11 encoded for 12 bit DACs"
     , GetOpt.Option [] ["randomData"]
        (GetOpt.NoArg (\opt -> return $ opt {optionsRandomData = True})) "Use a random number generator to create source data"
     , GetOpt.Option [] ["numberOfFrames"]
        (GetOpt.ReqArg (\iString opt -> return $ opt { optionsNumberOfFrames = read iString::Int }) "Number of Frames to generate" ) "Number of frames to generate before quiting. By default the program will run until terminated."
    ]
                
        
--Avoids throwing an EOF exception
safeReader :: Handle -> Int -> IO B.ByteString
safeReader handle size = do
    result <- hIsEOF handle
    if result then
        return B.empty
    else
        B.hGet handle size
        
--Entry Function to the program
main :: IO ()
main = do
    arguments <- getArgs
    
    case GetOpt.getOpt GetOpt.RequireOrder options arguments of
         
         (actions, [], []) -> do
             
             --Process Commandline input
             opt <- foldl (>>=) (return startOptions) actions
             
             --Some useful constants
             let constellationPower = discretePowerOf2 $ length $ optionsConstellationPoints opt
             let readLength = div (constellationPower * optionsNumberOfCarriers opt) 8
             let constellationArray = Array.array (0,fromIntegral (length $ optionsConstellationPoints opt) - 1) (optionsConstellationPoints opt)
             
             --Init the Thread Functions
             let dataReaderWorker = if optionsRandomData opt then
                                        readerThread (optionsDataInputReader opt) readLength
                                    else
                                        randomReaderThread readLength
                    
             let dataWriterWorker = writerThread (optionsSymbolOutputWriter opt) (optionsTransmitOutputWriter opt) 
             let dataWorker = workerThread (optionsNumberOfCarriers opt) (optionsCyclicPrefixLength opt) (optionsLengthOfSilence opt) (fromIntegral . toInteger $ constellationPower) constellationArray (optionsSCQ11Output opt)
             
             --Begin thread execution
             shardHandle <- RMS.shardResource dataReaderWorker (optionsNumberOfFrames opt) dataWriterWorker () dataWorker []
             
             --Wait for workers to terminate
             RMS.waitForCompletion shardHandle
             
             --Close File Handles
             optionsCloseTransmitOutputWriter opt
             optionsCloseSymbolOutputWriter opt
             optionsCloseDataInputReader opt
             
             --Quit
             exitSuccess
             
         (_, _, errors) -> do
             
              --Write Error
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              
              --Quit
              exitFailure
        

--Parses a list of 3-tuples into ordered pairs for use as a signal constellation when data encoding
programOptionsConstellationComprehender :: String -> [(Word8,Complex Double)]
programOptionsConstellationComprehender string = constellation
    
    where constellation = map (parser . splitOn ",") $ splitOn " " string 
          parser [x, y, z] = (read (tail x)::Word8, (read y::Double) :+ (read (init z)::Double) )
          parser _ = error "Incorrect constellation pattern."
        
--Reads Data from a file.
readerThread :: (Int -> IO B.ByteString) -> Int -> Int -> 
                        IO (Maybe (B.ByteString, Int))
readerThread _ _ 0 = return Nothing
readerThread blockReader blockSize state = do
    
    let newState = if state /= -1 then state - 1 else (-1)
    block <- blockReader blockSize
    if B.length block /= blockSize then
        return Nothing
        else
            return $ Just (block, newState)
        

--Generates random data instead of using a file
randomReaderThread :: Int -> Int -> IO (Maybe (B.ByteString, Int))
randomReaderThread _ 0 = return Nothing
randomReaderThread blockSize state = do
    
    source <- newStdGen
    
    let list = take blockSize (randoms source :: [Word8])
    
    let newState = if state /= -1 then state - 1 else (-1)
    
    return $ Just (B.pack list, newState)
    

    
--Writes two streams to a file
writerThread :: (B.ByteString -> IO ()) -> (B.ByteString -> IO ()) -> state
                    -> (B.ByteString, B.ByteString) -> IO (Maybe ())
writerThread symbolBlockWriter signalBlockWriter _ (symbolBlock, signalBlock) = do
    
    --Write symbol first since if both are being streamed to stdout then the the symbol goes first.
    symbolBlockWriter symbolBlock
    signalBlockWriter signalBlock
    
    return $ Just ()

    
--Encodes a data block into a signal
workerThread :: Int -> Int -> Int -> Int -> Array.Array Word8 (Complex Double)
                    -> Bool -> [Int] -> B.ByteString 
                    -> IO (Maybe ((B.ByteString, B.ByteString), [Int]))
workerThread numOfCarriers cyclicPrefixSize lengthOfSilence symbolSize constellationArray sc11Format _ dataBlock = do
    
    let symbol = SdrOFDMMod.encodeOFDMSymbol symbolSize constellationArray numOfCarriers dataBlock
    
    let transmitSignal = SdrOFDMMod.extendOFDMSymbol cyclicPrefixSize lengthOfSilence symbol
        
    return $ if sc11Format then
                Just ((serializeBlock complexFloatSerializer symbol, serializeBlock complexSC11Serializer transmitSignal),[])
             else 
                Just ((serializeBlock complexFloatSerializer symbol, serializeBlock complexFloatSerializer transmitSignal),[])
