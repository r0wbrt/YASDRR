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

module Shared.IO (
                                            BinaryParserExceptions
                                                ( DidNotExpectIncompleteData,
                                                    FailedWhileParsing,
                                                        UnhandledExtraInput),
            deserializeBlock, serializeBlock, blockListDeserializer,
            blockListSerializer, complexFloatSerializer,
            complexFloatDeserializer, complexSigned16Serializer,
            complexSigned16SerializerOne, complexSigned16DeserializerOne,
            complexDoubleSerializer, serializeBlockV, complexDoubleMagSerializer,
            complexDoubleDeserializer, complexSigned16Deserializer, serializeOutput,
            complexFloatMagSerializer,deserializeInput) where

-- System imports
import           Control.Exception
import qualified Control.Monad        as CM
import qualified Data.Binary.Get      as BG
import qualified Data.Binary.Put      as BP
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Complex
import           Data.Typeable
import qualified Data.Vector.Unboxed  as VUB
import           GHC.Float
import           Foreign.ForeignPtr    (newForeignPtr, newForeignPtr_, castForeignPtr)
import           Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (sizeOf)
import           System.IO.Unsafe      (unsafeDupablePerformIO)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.Vector.Storable  as VST
import           Foreign.C.String (CString)

--yasdrr shared imports
import qualified Shared.CommandLine   as CL


-- | Using the supplied sample format, converts a complex double vector into its
--   associated bytestring.
serializeOutput :: CL.SampleFormat -> VUB.Vector (Complex Double) -> B.ByteString
serializeOutput CL.SampleComplexDouble signal = serializeBlockV complexDoubleSerializer signal
serializeOutput CL.SampleComplexFloat signal = serializeBlockV complexFloatSerializer signal
serializeOutput CL.SampleComplexSigned16 signal = serializeBlockV complexSigned16SerializerOne signal
serializeOutput CL.SampleComplexToDoubleMag signal = serializeBlockV complexDoubleMagSerializer signal
serializeOutput CL.SampleComplexToFloatMag signal = serializeBlockV complexFloatMagSerializer signal
serializeOutput CL.SampleComplexToSigned16Mag signal = serializeBlockV complexSigned16MagSerializerOne signal



foreign import ccall unsafe "convertSigned16ArrayToDoubleArray" c_signed16ArrayToComplexDoubleArray ::
        CString
    ->  Ptr (Complex Double)
    ->  Int
    ->  IO ()

foreign import ccall unsafe "convertFloatArrayToDoubleArray" c_complexFloatArrayToComplexDoubleArray ::
        CString
    ->  Ptr (Complex Double)
    ->  Int
    ->  IO ()


deserializeInput :: CL.SampleFormat -> B.ByteString -> VUB.Vector (Complex Double)
deserializeInput CL.SampleComplexDouble bs = unsafeDupablePerformIO $ do
        bsPtr <- unsafeUseAsCStringLen bs ( \(src, _) -> return src)
        fPtr <- newForeignPtr_ bsPtr
        return $ VUB.convert $ VST.unsafeFromForeignPtr0 (castForeignPtr fPtr) arrayLength
    where arrayLength = quot (B.length bs) (sizeOf(undefined::Complex Double) )

deserializeInput CL.SampleComplexFloat bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLengthBytes arrayLength bs c_complexFloatArrayToComplexDoubleArray
          arrayLength = quot (B.length bs) (sizeOf(undefined::Complex Float))
          arrayLengthBytes = sizeOf(undefined::Complex Double) * arrayLength

deserializeInput CL.SampleComplexSigned16 bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLengthBytes arrayLength bs c_signed16ArrayToComplexDoubleArray
          arrayLength = quot (B.length bs)  4
          arrayLengthBytes = sizeOf(undefined::Complex Double) * arrayLength

deserializeInput _ _  = VUB.empty

{-# NOINLINE deserializeInputHelper #-}
deserializeInputHelper :: Int -> Int -> B.ByteString ->
                           (CString ->  Ptr (Complex Double) -> Int ->  IO ()) ->
                            IO (VUB.Vector (Complex Double))
deserializeInputHelper arrayLengthBytes arrayLength bs cFunc = do

        outputPtr <- mallocBytes arrayLengthBytes
        
        unsafeUseAsCStringLen bs $ do (\(src, _) -> cFunc src outputPtr (2*arrayLength))
        
        outputForeignPtr <- newForeignPtr finalizerFree outputPtr
        
        return $ VUB.convert $ VST.unsafeFromForeignPtr0 outputForeignPtr arrayLength


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
