

module Shared.IO where

import qualified YASDRR.IO.ComplexSerialization as IOComplex
import qualified Shared.CommandLine as CL
import Data.Complex
import qualified Data.Vector.Unboxed as VUB
import qualified Data.ByteString as B

serializeOutput :: CL.SampleFormat -> VUB.Vector (Complex Double) -> B.ByteString
serializeOutput format signal = case format of
                                   CL.SampleComplexDouble -> IOComplex.serializeBlockV IOComplex.complexDoubleSerializer signal
                                   CL.SampleComplexFloat -> IOComplex.serializeBlockV IOComplex.complexFloatSerializer signal
                                   CL.SampleComplexSigned16 -> IOComplex.serializeBlockV (IOComplex.complexSigned16Serializer 1.0) signal 
