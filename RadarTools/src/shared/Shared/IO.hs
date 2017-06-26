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
          serializeOutput
        , deserializeInput
        , complexFloatSerializer
        , complexSigned16SerializerOne
        , complexDoubleSerializer
        ) where




import qualified Data.Binary.Put        as BP
import qualified Data.ByteString        as B
import           Data.ByteString.Unsafe (unsafePackCStringFinalizer,
                                         unsafeUseAsCStringLen)
import           Data.Complex
import qualified Data.Vector.Storable   as VST
import           Data.Word
import           Foreign.C.String       (CString)
import           Foreign.ForeignPtr     (newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc  (finalizerFree, free, mallocBytes)
import           Foreign.Ptr            (Ptr, castPtr)
import           Foreign.Storable       (sizeOf)
import           GHC.Float
import qualified Shared.CommandLine     as CL
import           System.IO.Unsafe       (unsafeDupablePerformIO)



{- ### C Functions for serialization ### -}

-- | Converts a complex float array to an array of magnitudes.
foreign import ccall unsafe "convertComplexFloatArrayToDoubleMag" c_convertComplexFloatArrayToDoubleMag ::
        Ptr (Complex Float)
    ->  Ptr Double
    ->  Int
    ->  IO ()


-- | Converts a complex float array to an array of magnitudes stored as floats.
foreign import ccall unsafe "convertComplexFloatArrayToFloatMag" c_convertComplexFloatArrayToFloatMag ::
        Ptr (Complex Float)
    ->  Ptr Float
    ->  Int
    ->  IO ()


-- | Converts an array of complex float to an array to an array of their
--   magnitudes and stores them as a signed 16 values.
foreign import ccall unsafe "convertComplexFloatArrayToSigned16Mag" c_convertComplexFloatArrayToSigned16Mag ::
        Ptr (Complex Float)
    ->  Ptr Word16
    ->  Int
    ->  IO ()


-- | Makes a copy of the complex float array
foreign import ccall unsafe "convertFloatArrayToFloatArray" c_convertFloatArrayToFloatArray ::
        Ptr (Complex Float)
    ->  Ptr (Complex Float)
    ->  Int
    ->  IO ()


-- | Converts a complex float array to a complex signed 16 array using saturation
--  arithmetic.
foreign import ccall unsafe "convertComplexFloatArrayToComplexSigned16" c_convertComplexFloatArrayToComplexSigned16 ::
        Ptr (Complex Float)
    ->  Ptr Word16
    ->  Int
    ->  IO ()

-- | Makes a copy of a complex float array.
foreign import ccall unsafe "convertComplexFloatArrayToComplexDoubleArray" c_convertComplexFloatArrayToComplexDoubleArray ::
        Ptr (Complex Float)
    ->  Ptr (Complex Double)
    ->  Int
    ->  IO ()


-- | Using the supplied sample format, converts a complex float vector into its
--   associated bytestring.
serializeOutput :: CL.SampleFormat -> VST.Vector (Complex Float) -> B.ByteString
serializeOutput CL.SampleComplexDouble signal = runCBasedSerializer (sizeOf(undefined::Complex Double)) c_convertComplexFloatArrayToComplexDoubleArray signal
serializeOutput CL.SampleComplexFloat signal = runCBasedSerializer (sizeOf(undefined::Complex Float)) c_convertFloatArrayToFloatArray signal
serializeOutput CL.SampleComplexSigned16 signal =  runCBasedSerializer (sizeOf(undefined::Word32)) c_convertComplexFloatArrayToComplexSigned16 signal
serializeOutput CL.SampleComplexToDoubleMag signal = runCBasedSerializer (sizeOf(undefined::Double)) c_convertComplexFloatArrayToDoubleMag signal
serializeOutput CL.SampleComplexToFloatMag signal = runCBasedSerializer (sizeOf(undefined::Float)) c_convertComplexFloatArrayToFloatMag signal
serializeOutput CL.SampleComplexToSigned16Mag signal = runCBasedSerializer (sizeOf(undefined::Word16)) c_convertComplexFloatArrayToSigned16Mag signal


{- ### C Functions for deserialization ### -}

-- | Converts a double array into a float array
foreign import ccall unsafe "convertDoubleArrayToFloatArray" c_convertDoubleArrayToFloatArray ::
        CString
    ->  Ptr (Complex Float)
    ->  Int
    ->  IO ()


-- | Converts a float array to a float array. Note, size must be doubled.
foreign import ccall unsafe "convertComplexFloatArrayToComplexFloatArray" c_convertComplexFloatArrayToComplexFloatArray ::
        CString
    ->  Ptr (Complex Float)
    ->  Int
    ->  IO ()


-- | Converts a signed 16 array a float array. Note, the size must be doubled.
foreign import ccall unsafe "convertSigned16ArrayToFloatArray" c_signed16ArrayToComplexFloatArray ::
        CString
    ->  Ptr (Complex Float)
    ->  Int
    ->  IO ()


-- | Runs a C Based serializer that converts a Complex Float Unboxed Vector
--   into a ByteString.
{-# NOINLINE runCBasedSerializer #-}
runCBasedSerializer :: Int -> (Ptr (Complex Float)
    ->  Ptr a
    ->  Int
    ->  IO ()) -> VST.Vector (Complex Float) -> B.ByteString
runCBasedSerializer typeSize c_func signal = unsafeDupablePerformIO $ do
        outputPtr <- mallocBytes arraySize
        withForeignPtr sourcePtr $ \sPtr -> do
            c_func sPtr outputPtr (VST.length signal)
            unsafePackCStringFinalizer (castPtr outputPtr) arraySize (free outputPtr)

    where sourcePtr = fst $ VST.unsafeToForeignPtr0 signal
          arraySize = typeSize * VST.length signal


-- | Deserializes a ByteString into a complex float vector. Note, only supports
--   deserializing complex types. Magnitude types are not supported.
{-# NOINLINE deserializeInput #-}
deserializeInput :: CL.SampleFormat -> B.ByteString -> VST.Vector (Complex Float)
deserializeInput CL.SampleComplexDouble bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLength bs c_convertDoubleArrayToFloatArray
          arrayLength = quot (B.length bs) (sizeOf(undefined::Complex Double))

deserializeInput CL.SampleComplexFloat bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLength bs c_convertComplexFloatArrayToComplexFloatArray
          arrayLength = quot (B.length bs) (sizeOf(undefined::Complex Float))

deserializeInput CL.SampleComplexSigned16 bs = unsafeDupablePerformIO action
    where action = deserializeInputHelper arrayLength bs c_signed16ArrayToComplexFloatArray
          arrayLength = quot (B.length bs) (sizeOf(undefined::Word32))

deserializeInput _ _  = VST.empty


-- | Wrapper around common functionlaity used by deserializeInput.
{-# NOINLINE deserializeInputHelper #-}
deserializeInputHelper :: Int -> B.ByteString ->
                           (CString ->  Ptr (Complex Float) -> Int ->  IO ()) ->
                            IO (VST.Vector (Complex Float))
deserializeInputHelper arrayLength bs cFunc = do

        outputPtr <- mallocBytes arrayLengthBytes

        unsafeUseAsCStringLen bs $ \(src, _) ->
                cFunc src outputPtr (2*arrayLength)

        outputForeignPtr <- newForeignPtr finalizerFree outputPtr

        return $ VST.unsafeFromForeignPtr0 outputForeignPtr arrayLength

    where arrayLengthBytes = sizeOf(undefined::Complex Float) * arrayLength


-- | Serializes a complex float.
complexFloatSerializer :: Complex Float -> BP.Put
complexFloatSerializer (real :+ imaginary) = do

    BP.putFloatle real
    BP.putFloatle imaginary

-- | Serializes a Complex Float into a signed 16 with base 1.
--   Faster then complexSigned16Serializer since no division is performed.
complexSigned16SerializerOne :: Complex Float -> BP.Put
complexSigned16SerializerOne (real :+ imaginary) = do
    BP.putInt16host $ convertNumber real
    BP.putInt16host $ convertNumber imaginary

    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number, 32767.0])

-- | Serializes a complex Float as a complex double
complexDoubleSerializer :: Complex Float -> BP.Put
complexDoubleSerializer (real :+ imaginary) = do
    BP.putDoublele  (float2Double real)
    BP.putDoublele  (float2Double imaginary)
