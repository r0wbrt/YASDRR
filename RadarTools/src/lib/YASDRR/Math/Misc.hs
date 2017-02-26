--Copyright Robert C. Taylor

{- |
Module      :  YASDRR.Math.Misc
Description :  Misc math functions
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

Several mathmatical functions used as primitives by other more complex 
operations in YASDRR tools library.
-}

module YASDRR.Math.Misc where

import Data.Bits
import qualified Data.Vector as V

--Converts a value to the nearest value of 2 assuming the value is a power of 2.
discretePowerOf2 :: Int -> Int
discretePowerOf2 number
    | setBit 0 lowerValue < number  = lowerValue + 1
    | otherwise                     = lowerValue
    where lowerValue = (finiteBitSize number - 1) - countLeadingZeros number
 
getVectorMatrixRow :: Int -> V.Vector (V.Vector a) -> V.Vector a
getVectorMatrixRow rowNumber = V.map (V.! rowNumber)


