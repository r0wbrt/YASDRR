
module YASDRR.IO.LazyComplexSerialization (
    vToComplexDoubleByteString, vToComplexDoubleByteStream,
    vToComplexFloatByteString, vToComplexFloatByteStream,
    vToComplexSigned16ByteString, vToComplexSigned16ByteStream) where

import System.IO
import qualified Data.ByteString.Builder as BB
import qualified Data.Monoid as DM
import qualified Data.Vector.Unboxed as VUB
import qualified Data.ByteString.Lazy as BL
import Data.Complex
import GHC.Float


vToComplexDoubleByteString :: VUB.Vector (Complex Double) -> BL.ByteString
vToComplexDoubleByteStream :: Handle -> VUB.Vector (Complex Double) -> IO ()

vToComplexFloatByteString :: VUB.Vector (Complex Double) -> BL.ByteString
vToComplexFloatByteStream :: Handle -> VUB.Vector (Complex Double) -> IO ()

vToComplexSigned16ByteString :: Double -> VUB.Vector (Complex Double) -> BL.ByteString
vToComplexSigned16ByteStream :: Handle -> Double -> VUB.Vector (Complex Double) -> IO ()

vToByteString :: (Complex Double -> BB.Builder) -> VUB.Vector (Complex Double) -> BL.ByteString
vToByteStream :: (Complex Double -> BB.Builder) -> Handle -> VUB.Vector (Complex Double) -> IO ()

complexDoubleSerializer :: (Complex Double) -> BB.Builder
complexFloatSerializer :: (Complex Double) -> BB.Builder
complexSigned16Serializer :: Double -> (Complex Double) -> BB.Builder



vToComplexDoubleByteString vector = vToByteString complexDoubleSerializer vector
vToComplexDoubleByteStream h vector = vToByteStream complexDoubleSerializer h vector


vToComplexFloatByteString vector = vToByteString complexFloatSerializer vector
vToComplexFloatByteStream h vector = vToByteStream complexFloatSerializer h vector


vToComplexSigned16ByteString base vector = vToByteString (complexSigned16Serializer base) vector
vToComplexSigned16ByteStream h base vector = vToByteStream (complexSigned16Serializer base) h vector


vToByteStream mode h vector = BB.hPutBuilder h (DM.mconcat $ map (mode) $ VUB.toList vector)
vToByteString mode vector = BB.toLazyByteString (DM.mconcat $ map (mode) $ VUB.toList vector)


complexDoubleSerializer (real :+ imag) = BB.doubleLE real DM.<> BB.doubleLE imag
complexFloatSerializer (real :+ imag) = BB.floatLE (double2Float real) DM.<> BB.floatLE (double2Float imag)


complexSigned16Serializer base (real :+ imaginary) = BB.int16LE (convertNumber real) DM.<> BB.int16LE (convertNumber imaginary)
    where convertNumber number = floor $ signum number * abs (minimum [ 32767.0 * number / base, 32767.0])
