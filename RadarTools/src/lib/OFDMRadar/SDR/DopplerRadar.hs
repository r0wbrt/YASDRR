
-- Copyright Robert C. Taylor

module OFDMRadar.SDR.DopplerRadar where

import qualified Data.Vector as V
import Data.List
import OFDMRadar.DSP.FFT
import OFDMRadar.Math.Misc

--Process a doppler return given the pulses stored as a 2-d vector of vectors.
processDopplerReturnV pulseList = V.map fft transposedMatrix
    
    where fft = createFftV (V.length $ transposedMatrix V.! 0) (-1)
          
          transposedMatrix = V.generate numberOfRows (\rowN -> getVectorMatrixRow rowN pulseList)
          
          numberOfRows = V.length (pulseList V.! 0)
          
--Process a doppler return given the pulses stores as a 2-d list of list.
processDopplerReturn pulseList  = map fft transposedMatrix
    
    where fft = createFft (length (head transposedMatrix)) (-1)
          
          transposedMatrix = transpose pulseList
          
