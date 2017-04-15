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
Module      :  YASDRR.SDR.DopplerRadar
Description :  Functions that can be used to build a doppler radar.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable
Portability :  portable

Primitive functions that are used to write functions and programs that perform
doppler processing on radar input.
-}

module YASDRR.SDR.DopplerRadar
    ( processDopplerReturn
    , processDopplerReturnV
    )
    where

import           Data.Complex
import           Data.List
import qualified Data.Vector         as VB
import qualified Data.Vector.Unboxed as VUB
import           YASDRR.DSP.FFT
import           YASDRR.Math.Misc

--Process a doppler return given the pulses stored as a 2-d vector of vectors.
processDopplerReturnV :: VB.Vector ( VUB.Vector (Complex Double) ) ->
                                            VB.Vector (VUB.Vector (Complex Double))
processDopplerReturnV pulseList = VB.map fft transposedMatrix

    where fft = createFftV (VUB.length $ transposedMatrix VB.! 0) (-1)

          transposedMatrix = VB.generate numberOfRows grabRow

          grabRow = flip getVectorMatrixRow pulseList

          numberOfRows = VUB.length (pulseList VB.! 0)

--Process a doppler return given the pulses stores as a 2-d list of list.
processDopplerReturn :: [[Complex Double]] -> [[Complex Double]]
processDopplerReturn pulseList  = map fft transposedMatrix

    where fft = createFft (length (head transposedMatrix)) (-1)

          transposedMatrix = transpose pulseList

