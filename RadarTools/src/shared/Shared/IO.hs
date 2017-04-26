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
Module      :  Shared.IO
Description :  Serialization and deserialization code
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable
Portability :  portable

-}

module Shared.IO
    (
        BinaryParserExceptions
          ( DidNotExpectIncompleteData
          , FailedWhileParsing
          , UnhandledExtraInput
          )
        , deserializeBlock
        , serializeBlock
        , blockListDeserializer
        , blockListSerializer
        , complexSigned16MagSerializerOne
        , complexFloatSerializer
        , complexFloatDeserializer
        , complexSigned16Serializer
        , complexSigned16SerializerOne
        , complexSigned16DeserializerOne
        , complexDoubleSerializer
        , serializeBlockV
        , complexDoubleMagSerializer
        , complexDoubleDeserializer
        , complexSigned16Deserializer
        , serializeOutput
        , complexFloatMagSerializer
        , deserializeInput
        ) where

import           Control.Exception
import qualified Control.Monad          as CM
import qualified Data.Binary.Get        as BG
import qualified Data.Binary.Put        as BP
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.ByteString.Unsafe (unsafePackCStringFinalizer,
                                         unsafeUseAsCStringLen)
import           Data.Complex
import           Data.Typeable
import qualified Data.Vector.Storable   as VST
import qualified Data.Vector.Unboxed    as VUB
import           Data.Word
import           Foreign.C.String       (CString)
import           Foreign.ForeignPtr     (newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc  (free, finalizerFree, mallocBytes)
import           Foreign.Ptr            (Ptr, castPtr)
import           Foreign.Storable       (peekElemOff, sizeOf)
import           GHC.Float
import qualified Shared.CommandLine     as CL
import           System.IO.Unsafe       (unsafeDupablePerformIO)


-- | Using the supplied sample format, converts a complex double vector into its
--   associated bytestring.
serializeOutput :: CL.SampleFormat -> VST.Vector (Complex Double) -> B.ByteString
serializeOutput CL.SampleComplexDouble signal = runCBasedSerializer (sizeOf(undefined::Complex Double)) c_convertComplexDoubleArrayToComplexDoubleArray signal
serializeOutput CL.SampleComplexFloat signal = runCBasedSerializer (sizeOf(undefined::Complex Float)) c_convertComplexDoubleArrayToComplexFloatArray signal
serializeOutput CL.SampleComplexSigned16 signal =  runCBasedSerializer (sizeOf(undefined::Word32)) c_convertComplexDoubleArrayToComplexSigned16 signal
serializeOutput CL.SampleComplexToDoubleMag signal = runCBasedSerializer (sizeOf(undefined::Double)) c_convertComplexDoubleArrayToDoubleMag signal
serializeOutput CL.SampleComplexToFloatMag signal = runCBasedSerializer (sizeOf(undefined::Float)) c_convertComplexDoubleArrayToFloatMag signal
serializeOutput CL.SampleComplexToSigned16Mag signal = runCBasedSerializer (sizeOf(undefined::Word16)) c_convertComplexDoubleArrayToSigned16Mag signal

-- | Converts a signed 16 array a double array. Note, the size must be doubled.
foreign import ccall unsafe "convertSigned16ArrayToDoubleArray" c_signed16ArrayToComplexDoubleArray ::
        CString
    ->  Ptr (Complex Double)
    ->  Int
    ->  IO ()


-- | Converts a float array to a double array. Note, size must be doubled.
foreign import ccall unsafe "convertFloatArrayToDoubleArray" c_complexFloatArrayToComplexDoubleArray ::
        CString
    ->  Ptr (Complex Double)
    ->  Int
    ->  IO ()


-- | Converts a complex double array to an array of magnitudes.
foreign import ccall unsafe "convertComplexDoubleArrayToDoubleMag" c_convertComplexDoubleArrayToDoubleMag ::
        Ptr (Complex Double)
    ->  Ptr Double
    ->  Int
    ->  IO ()


-- | Converts a complex double array to an array of magnitudes stored as floats.
foreign import ccall unsafe "convertComplexDoubleArrayToFloatMag" c_convertComplexDoubleArrayToFloatMag ::
        Ptr (Complex Double)
    ->  Ptr Float
    ->  Int
    ->  IO ()


-- | Converts an array of complex doubles to an array to an array of their
--   magnitudes and stores them as a signed 16 values.
foreign import ccall unsafe "convertComplexDoubleArrayToSigned16Mag" c_convertComplexDoubleArrayToSigned16Mag ::
        Ptr (Complex Double)
    ->  Ptr Word16
    ->  Int
    ->  IO ()


-- | Converts a complex double array to a complex float array.
foreign import ccall unsafe "convertComplexDoubleArrayToComplexFloatArray" c_convertComplexDoubleArrayToComplexFloatArray ::
        Ptr (Complex Double)
    ->  Ptr (Complex Float)
    ->  Int
    ->  IO ()


-- | Converts a complex double array to a complex signed 16 array using saturation
--  arithmetic.
foreign import ccall unsafe "convertComplexDoubleArrayToComplexSigned16" c_convertComplexDoubleArrayToComplexSigned16 ::
        Ptr (Complex Double)
    ->  Ptr Word16
    ->  Int
    ->  IO ()

-- | Makes a copy of a complex double array.
foreign import ccall unsafe "convertComplexDoubleArrayToComplexDoubleArray" c_convertComplexDoubleArrayToComplexDoubleArray ::
        Ptr (Complex Double)
    ->  Ptr (Complex Double)
    ->  Int
    ->  IO ()

-- | Runs a C Based serializer that converts a Complex Double Unboxed Vector
--   into a ByteString.
{-# NOINLINE runCBasedSerializer #-}
runCBasedSerializer :: Int -> (Ptr (Complex Double)
    ->  Ptr a
    ->  Int
    ->  IO ()) -> VST.Vector (Complex Double) -> B.ByteString
runCBasedSerializer typeSize c_func signal = unsafeDupablePerformIO $ do
        outputPtr <- mallocBytes arraySize
        withForeignPtr sourcePtr $ \sPtr -> do
            c_func sPtr outputPtr (VST.length signal)
            unsafePackCStringFinalizer (castPtr outputPtr) arraySize (free outputPtr)

    where sourcePtr = fst $ VST.unsafeToForeignPtr0 signal
          arraySize = typeSize * VST.length signal

-- | Deserializes a ByteString into a complex double vector. Note, only supports
--   deserializing complex types. Magnitude types are not supported.
{-# NOINLINE deserializeInput #-}
deserializeInput :: CL.SampleFormat -> B.ByteString -> VST.Vector (Complex Double)
deserializeInput CL.SampleComplexDouble bs = unsafeDupablePerformIO $
        unsafeUseAsCStringLen bs $ \(bsPtr, _) ->
            return $! VST.generate arrayLength (unsafeDupablePerformIO . peekElemOff (castPtr bsPtr))

    where arrayLength = quot (B.length bs) (sizeOf(undefined::Complex Double))


deserializeInput CL.SampleComplexFloat bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLength bs c_complexFloatArrayToComplexDoubleArray
          arrayLength = quot (B.length bs) (sizeOf(undefined::Complex Float))


deserializeInput CL.SampleComplexSigned16 bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLength bs c_signed16ArrayToComplexDoubleArray
          arrayLength = quot (B.length bs) (sizeOf(undefined::Word32))

deserializeInput _ _  = VST.empty


-- | Wrapper around common functionlaity used by deserializeInput.
{-# NOINLINE deserializeInputHelper #-}
deserializeInputHelper :: Int -> B.ByteString ->
                           (CString ->  Ptr (Complex Double) -> Int ->  IO ()) ->
                            IO (VST.Vector (Complex Double))
deserializeInputHelper arrayLength bs cFunc = do

        outputPtr <- mallocBytes arrayLengthBytes
        
        unsafeUseAsCStringLen bs $ \(src, _) ->
                cFunc src outputPtr (2*arrayLength)
                
        outputForeignPtr <- newForeignPtr finalizerFree outputPtr

        return $ VST.unsafeFromForeignPtr0 outputForeignPtr arrayLength

    where arrayLengthBytes = sizeOf(undefined::Complex Double) * arrayLength


-- | List of exceptions deserializeBlock can throw to consuming code.
data BinaryParserExceptions = DidNotExpectIncompleteData String
                            | FailedWhileParsing String
                            | UnhandledExtraInput String
    deriving (Show, Typeable)
instance Exception BinaryParserExceptions


-- |  Helper function for deserializing a block of data.
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


-- | Converts a binary string into the requested data type.
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


-- | Converts a list of data into it associated ByteString type.
serializeBlock :: (a -> BP.Put) -> [a] -> B.ByteString
serializeBlock encoder block = BL.toStrict $ BP.runPut puter

    where puter = serializationStreamProcessor encoder block


-- | Converts a vector of data into its associated type.
serializeBlockV :: (VUB.Unbox a) => (a -> BP.Put) -> VUB.Vector a -> B.ByteString
serializeBlockV encoder block = BL.toStrict $ BP.runPut puter

    where puter = serializationStreamProcessorV encoder block


-- | Helper function for serializing a vector into a byte string.
serializationStreamProcessorV :: (VUB.Unbox a) => (a -> BP.Put) -> VUB.Vector a -> BP.Put
serializationStreamProcessorV parser vector = do
    parser $ VUB.unsafeHead vector
    CM.when (VUB.length vector > 1) $ serializationStreamProcessorV parser (VUB.tail vector)


-- | Takes an input bytestring and converts it into a list of blocks, where each
--   block is a list with the requested size of the desired type.
blockListDeserializer :: BG.Get a -> Int -> BG.Get [a]
blockListDeserializer _ 0 = return []
blockListDeserializer decoder size = do
    signalHead <- decoder
    signalTail <- blockListDeserializer decoder (size - 1)
    return (signalHead:signalTail)


-- | Takes a list of lists of a specified type and then serializes the data into
--   a bytestring.
blockListSerializer :: (a -> BP.Put) -> [a] -> BP.Put
blockListSerializer _ [] = return ()
blockListSerializer encoder input = do

    encoder (head input)

    blockListSerializer encoder (tail input)


-- | Serializes a complexDouble.
complexDoubleSerializer :: Complex Double -> BP.Put
complexDoubleSerializer (real :+ imaginary) = do
    BP.putDoublele real
    BP.putDoublele imaginary


-- | Serializes a complex double list into a list of squared complex double magnitudes |z|^2.
complexDoubleMagSerializer :: Complex Double -> BP.Put
complexDoubleMagSerializer (real :+ imaginary) = BP.putDoublele $ real*real + imaginary*imaginary


-- | Serializes a Complex Double into a signed 16 with arbitrary base.
complexSigned16Serializer :: Double -> Complex Double -> BP.Put
complexSigned16Serializer base (real :+ imaginary) = do
    BP.putInt16host $ convertNumber real
    BP.putInt16host $ convertNumber imaginary

    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number / base, 32767.0])


-- | Serializes a Complex Double into a signed 16 with base 1.
--   Faster then complexSigned16Serializer since no division is performed.
complexSigned16SerializerOne :: Complex Double -> BP.Put
complexSigned16SerializerOne (real :+ imaginary) = do
    BP.putInt16host $ convertNumber real
    BP.putInt16host $ convertNumber imaginary

    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number, 32767.0])


-- | Serializes a complex double list into a list of squared complex signed 16 magnitudes |z|^2.
complexSigned16MagSerializerOne :: Complex Double -> BP.Put
complexSigned16MagSerializerOne (real :+ imaginary) = BP.putInt16host $ convertNumber $ real*real + imaginary*imaginary

    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number, 32767.0])


-- | Deserializes a complex signed 16 into a complex double using a base of 1.
complexSigned16DeserializerOne :: BG.Get (Complex Double)
complexSigned16DeserializerOne = do

    --First parse the data from the stream
    real <- BG.getWord16le
    imaginary <- BG.getWord16le

    --Next scale the data accordingly
    let realD = fromIntegral real / 32768.0
    let imagD = fromIntegral imaginary / 32768.0

    --Return the result.
    return (realD :+ imagD)


-- | Serializes a complex float.
complexFloatSerializer :: Complex Double -> BP.Put
complexFloatSerializer (real :+ imaginary) = do

    BP.putFloatle (double2Float real)
    BP.putFloatle (double2Float imaginary)


-- | Serializes a complex double list into a list of squared complex float magnitudes |z|^2.
complexFloatMagSerializer :: Complex Double -> BP.Put
complexFloatMagSerializer (real :+ imaginary) = BP.putFloatle $ double2Float (real*real + imaginary*imaginary)


-- | Deserializes a complex float.
complexFloatDeserializer :: BG.Get (Complex Double)
complexFloatDeserializer = do

    real <- BG.getFloatle
    imaginary <- BG.getFloatle

    return (float2Double real :+ float2Double imaginary)


-- | Deserializes a complex double
complexDoubleDeserializer :: BG.Get (Complex Double)
complexDoubleDeserializer = do

    real <- BG.getDoublele
    imaginary <- BG.getDoublele

    return (real :+ imaginary)


-- | Serializes a complex double to a signed 16 using an arbitrary base.
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
