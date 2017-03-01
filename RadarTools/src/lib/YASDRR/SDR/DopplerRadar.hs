-- Copyright Robert C. Taylor

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

module YASDRR.SDR.DopplerRadar where

import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VUB
import Data.List
import YASDRR.DSP.FFT
import YASDRR.Math.Misc
import Data.Complex

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
          
