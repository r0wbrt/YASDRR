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
Module      :  YASDRR.DSP.Correlation
Description :  Functions used to calculate the correlation between two
               sequences of complex numbers.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable
Portability :  portable

Contains several useful functions to calculate the correlation of two complex
number sequences.
-}

module YASDRR.DSP.Correlation
    (
      correlate
    )  where

import           Data.Complex
import qualified Data.Vector.Storable  as VST
import           Foreign.ForeignPtr    (newForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (sizeOf)
import           System.IO.Unsafe      (unsafeDupablePerformIO)


foreign import ccall unsafe "complexCorrelate" c_correlation ::
        Int
    ->  Int
    ->  Ptr (Complex Float)
    ->  Ptr (Complex Float)
    ->  Ptr (Complex Float)
    ->  IO ()

-- | Does complex correlation.
{-# NOINLINE correlate #-}
correlate :: VST.Vector (Complex Float) -> VST.Vector (Complex Float)
                    -> VST.Vector (Complex Float)
correlate impulse pulse = unsafeDupablePerformIO $ do

    outputPtr <- mallocBytes $ sizeOf(undefined::Complex Float) * pulseSize

    _ <- withForeignPtr pulsePtr (\pulseRawPtr ->
        withForeignPtr impulsePtr (\impulseRawPtr ->
            c_correlation impulseSize pulseSize impulseRawPtr pulseRawPtr outputPtr))

    outputForeignPtr <- newForeignPtr finalizerFree outputPtr

    return $ VST.unsafeFromForeignPtr0 outputForeignPtr pulseSize

    where pulsePtr = fst $ VST.unsafeToForeignPtr0 pulse
          impulsePtr = fst $ VST.unsafeToForeignPtr0 impulse
          impulseSize = VST.length impulse
          pulseSize = VST.length pulse

