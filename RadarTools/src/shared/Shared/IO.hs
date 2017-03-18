

module Shared.IO where

import qualified YASDRR.IO.ComplexSerialization as IOComplex
import qualified Shared.CommandLine as CL
import Data.Complex
import qualified Data.Vector.Unboxed as VUB
import qualified Data.ByteString as B
import qualified YASDRR.IO.LazyComplexSerialization as LCS
import System.IO

serializeOutput :: CL.SampleFormat -> VUB.Vector (Complex Double) -> B.ByteString
serializeOutput format signal = case format of
                                   CL.SampleComplexDouble -> IOComplex.serializeBlockV IOComplex.complexDoubleSerializer signal
                                   CL.SampleComplexFloat -> IOComplex.serializeBlockV IOComplex.complexFloatSerializer signal
                                   CL.SampleComplexSigned16 -> IOComplex.serializeBlockV (IOComplex.complexSigned16Serializer 1.0) signal 

lazySerializeToOutputStream :: Handle -> CL.SampleFormat -> VUB.Vector (Complex Double) -> IO ()
lazySerializeToOutputStream h format signal = case format of
                                   CL.SampleComplexDouble -> LCS.vToComplexDoubleByteStream h signal
                                   CL.SampleComplexFloat -> LCS.vToComplexFloatByteStream h signal
                                   CL.SampleComplexSigned16 -> LCS.vToComplexSigned16ByteStream h 1.0 signal
