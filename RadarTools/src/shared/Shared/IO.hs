

module Shared.IO (
                                            BinaryParserExceptions 
                                                ( DidNotExpectIncompleteData,
                                                    FailedWhileParsing,
                                                        UnhandledExtraInput),
            deserializeBlock, serializeBlock, blockListDeserializer,
            blockListSerializer, complexFloatSerializer,
            complexFloatDeserializer, complexSC11Deserializer,
            complexSC11Serializer, complexSigned16Serializer, 
            complexSigned16SerializerOne, complexDoubleSerializer, 
            serializeBlockV,
            complexDoubleDeserializer, complexSigned16Deserializer, serializeOutput) where

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Complex
import GHC.Float
import Control.Exception
import Data.Typeable
import qualified Data.Vector.Unboxed as VUB
import qualified Control.Monad as CM
import qualified Shared.CommandLine as CL



serializeOutput :: CL.SampleFormat -> VUB.Vector (Complex Double) -> B.ByteString
serializeOutput format signal = case format of
                                   CL.SampleComplexDouble -> serializeBlockV complexDoubleSerializer signal
                                   CL.SampleComplexFloat -> serializeBlockV complexFloatSerializer signal
                                   CL.SampleComplexSigned16 -> serializeBlockV (complexSigned16SerializerOne) signal 



data BinaryParserExceptions = DidNotExpectIncompleteData String 
                            | FailedWhileParsing String 
                            | UnhandledExtraInput String  
    deriving (Show, Typeable)
instance Exception BinaryParserExceptions

deserializationStreamProcessor :: BG.Get a -> [a] -> BG.Get [a]
deserializationStreamProcessor decoder acc = do
    
    noMoreData <- BG.isEmpty

    if noMoreData then
         return (reverse acc)
         
    else 
        do
            --Grab output
            sample <- decoder
            deserializationStreamProcessor decoder (sample:acc)

deserializeBlock :: BG.Get a -> B.ByteString -> ([a], B.ByteString)
deserializeBlock decoder block = 
        
    --Run the decoder and handle it three possible output.
    case BG.pushEndOfInput $ BG.pushChunk bgdecoder block of
         
        --Something went wrong
        BG.Fail _ _ msg -> throw $ FailedWhileParsing msg
        
        --Not all the data was passed in.
        BG.Partial _ -> throw $ DidNotExpectIncompleteData "Expected parsing to be complete, however decoder is still expecting more input"
        
        --All data was passed in and decoded.
        BG.Done extraData _ sampleSequence -> (sampleSequence, extraData)                                                
        
    where bgdecoder = BG.runGetIncremental (deserializationStreamProcessor decoder [])
 
 
-- | Parses a list of types into a byte stream
serializationStreamProcessor :: (a -> BP.Put) -> [a] -> BP.Put
serializationStreamProcessor _ [] = return ()
serializationStreamProcessor parser (x:xs) = do
    parser x 
    serializationStreamProcessor parser xs
    
serializeBlock :: (a -> BP.Put) -> [a] -> B.ByteString
serializeBlock encoder block = BL.toStrict $ BP.runPut puter
                                              
    where puter = serializationStreamProcessor encoder block
          
          
serializeBlockV :: (VUB.Unbox a) => (a -> BP.Put) -> VUB.Vector a -> B.ByteString
serializeBlockV encoder block = BL.toStrict $ BP.runPut puter
                                              
    where puter = serializationStreamProcessorV encoder block
    
    
serializationStreamProcessorV :: (VUB.Unbox a) => (a -> BP.Put) -> VUB.Vector a -> BP.Put
serializationStreamProcessorV parser vector = do
    parser $ VUB.unsafeHead vector 
    CM.when (VUB.length vector > 1) $ serializationStreamProcessorV parser (VUB.tail vector) 
    
    
blockListDeserializer :: BG.Get a -> Int -> BG.Get [a]
blockListDeserializer _ 0 = return []
blockListDeserializer decoder size = do
    signalHead <- decoder
    signalTail <- blockListDeserializer decoder (size - 1)
    return (signalHead:signalTail)

    
blockListSerializer :: (a -> BP.Put) -> [a] -> BP.Put
blockListSerializer _ [] = return ()
blockListSerializer encoder input = do
    
    encoder (head input)
    
    blockListSerializer encoder (tail input)
    
    
complexDoubleSerializer :: Complex Double -> BP.Put
complexDoubleSerializer (real :+ imaginary) = do
    BP.putDoublele real
    BP.putDoublele imaginary
    
    
complexSigned16Serializer :: Double -> Complex Double -> BP.Put
complexSigned16Serializer base (real :+ imaginary) = do
    BP.putInt16host $ convertNumber real
    BP.putInt16host $ convertNumber imaginary
    
    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number / base, 32767.0])
          
          
complexSigned16SerializerOne :: Complex Double -> BP.Put
complexSigned16SerializerOne (real :+ imaginary) = do
    BP.putInt16host $ convertNumber real
    BP.putInt16host $ convertNumber imaginary
    
    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number, 32767.0])
    
    
complexFloatSerializer :: Complex Double -> BP.Put
complexFloatSerializer (real :+ imaginary) = do
    
    BP.putFloatle (double2Float real)
    BP.putFloatle (double2Float imaginary)

    
complexFloatDeserializer :: BG.Get (Complex Double)
complexFloatDeserializer = do
    
    real <- BG.getFloatle 
    imaginary <- BG.getFloatle
    
    return (float2Double real :+ float2Double imaginary)
    
    
complexDoubleDeserializer :: BG.Get (Complex Double)
complexDoubleDeserializer = do
    
    real <- BG.getDoublele 
    imaginary <- BG.getDoublele
    
    return (real :+ imaginary)
    
    
complexSigned16Deserializer :: Double -> BG.Get (Complex Double)
complexSigned16Deserializer base = do
    
    --First parse the data from the stream
    real <- BG.getWord16le
    imaginary <- BG.getWord16le
    
    --Next scale the data accordingly
    let realD = (fromIntegral real * base) / 32768.0
    let imagD = (fromIntegral imaginary * base) / 32768.0
    
    --Return the result.
    return (realD :+ imagD)
    
    
complexSC11Serializer :: Complex Double -> BP.Put
complexSC11Serializer (real :+ imaginary) = do
    
    BP.putInt16le $ convertNumber real
    BP.putInt16le $ convertNumber imaginary
    
    --Helper function to scale and then convert the number
    where convertNumber number = floor $ signum number * abs (minimum [2047.0 * number, 2047.0])


complexSC11Deserializer :: BG.Get (Complex Double)
complexSC11Deserializer = do
    
    --First parse the data from the stream
    real <- BG.getWord16le
    imaginary <- BG.getWord16le
    
    --Next scale the data accordingly
    let realD = fromIntegral real / 2048.0
    let imagD = fromIntegral imaginary / 2048.0
    
    --Return the result.
    return (realD :+ imagD)
