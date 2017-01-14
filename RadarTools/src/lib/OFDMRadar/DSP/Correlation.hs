--Copyright Robert C. Taylor - All Rights Reserved

module OFDMRadar.DSP.Correlation (correlate, correlateV) where

import qualified Data.Vector as V
import Data.Complex
import Data.Array

-- | Vector based correlation using complex numbers.
correlateV impulse pulse
    | impulse == V.empty = pulse
    | otherwise = V.generate (V.length pulse) (compress)
    where compress offset = V.sum $ V.zipWith (*) conjImpulse (V.drop offset pulse)
          conjImpulse = V.map (conjugate) impulse

          
-- | Complex correlation over a list.
correlate impulse signal = map (correlateLoop conjImpulse signalArray (length signal) 0) [0..((length signal)-1)]
    where conjImpulse = (map (conjugate) impulse)
          signalArray = array (0, length signal) $ zip [0,1..] signal

--Correlation accumulator loop, written based on profiling data
correlateLoop [] _ _ acc _  = acc `seq` acc
correlateLoop impulse signal signalSize acc offset 
    | (offset == signalSize) = acc `seq` acc
    | otherwise = acc `seq` (correlateLoop (tail impulse) signal signalSize ( ( (signal!offset) * (head impulse) ) + acc) (offset + 1))
