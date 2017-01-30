--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  OFDMRadar.DSP.Correlation
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

module OFDMRadar.DSP.Correlation (correlate, correlateV) where

import qualified Data.Vector as V
import Data.Complex
import Data.Array

-- | Vector based correlation using complex numbers.
correlateV :: V.Vector (Complex Double) -> V.Vector (Complex Double) 
                    -> V.Vector (Complex Double)
correlateV impulse pulse
    | impulse == V.empty = pulse
    | otherwise = V.generate (V.length pulse) compress
    where compress offset = V.sum $ V.zipWith (*) 
                                        conjImpulse (V.drop offset pulse)
          conjImpulse = V.map conjugate impulse

          
-- | Complex correlation over a list.
correlate :: [Complex Double] -> [Complex Double] -> [Complex Double]
correlate impulse signal = map loopFunction [0..(length signal - 1)]
                                    
    where conjImpulse = map conjugate impulse
          loopFunction = correlateLoop conjImpulse signalArray (length signal) 0
          signalArray = array (0, length signal) $ zip [0,1..] signal

--Correlation accumulator loop, written based on profiling data
correlateLoop :: [Complex Double] -> Array Int (Complex Double) -> Int 
                        -> Complex Double -> Int -> Complex Double
correlateLoop [] _ _ acc _  = acc
correlateLoop impulse signal signalSize acc offset 
    | offset == signalSize = acc
    | otherwise = acc `seq` correlateLoop (tail impulse) signal signalSize 
                    ( ( (signal!offset) * head impulse ) + acc)
                    (offset + 1)
