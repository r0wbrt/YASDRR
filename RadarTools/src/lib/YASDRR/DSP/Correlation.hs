--Copyright Robert C. Taylor - All Rights Reserved

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

module YASDRR.DSP.Correlation (correlate, correlateV) where

import qualified Data.Vector.Unboxed as VUB
import Data.Complex

-- | Vector based correlation using complex numbers.
correlateV :: VUB.Vector (Complex Double) -> VUB.Vector (Complex Double) 
                    -> VUB.Vector (Complex Double)
correlateV impulse pulse
    | impulse == VUB.empty = pulse
    | otherwise = VUB.generate (VUB.length pulse) compress
    where compress offset = VUB.sum $ VUB.zipWith (*) 
                                        conjImpulse (VUB.unsafeDrop offset pulse)
          conjImpulse = VUB.map conjugate impulse

          
-- | Complex correlation over a list.
correlate :: [Complex Double] -> [Complex Double] -> [Complex Double]
correlate impulse signal = map loopFunction [0..(length signal - 1)]
                                    
    where conjImpulse = map conjugate impulse
          loopFunction = correlateLoop conjImpulse signalArray (length signal) 0
          signalArray = VUB.fromList signal

--Correlation accumulator loop, written based on profiling data
correlateLoop :: [Complex Double] -> VUB.Vector (Complex Double) -> Int 
                        -> Complex Double -> Int -> Complex Double
correlateLoop [] _ _ acc _  = acc
correlateLoop impulse signal signalSize acc offset
    | offset == signalSize = acc
    | otherwise = acc `seq` correlateLoop (tail impulse) signal signalSize 
                    ( ( VUB.unsafeIndex signal offset * head impulse ) + acc)
                    (offset + 1)
