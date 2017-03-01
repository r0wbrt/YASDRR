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
import qualified Data.Vector.Unboxed as VUB

--Converts a value to the nearest value of 2 assuming the value is a power of 2.
discretePowerOf2 :: Int -> Int
discretePowerOf2 number
    | setBit 0 lowerValue < number  = lowerValue + 1
    | otherwise                     = lowerValue
    where lowerValue = (finiteBitSize number - 1) - countLeadingZeros number
 
getVectorMatrixRow :: (VUB.Unbox a) => Int -> V.Vector (VUB.Vector a) -> VUB.Vector a
getVectorMatrixRow rowNumber matrix = VUB.generate (VUB.length (matrix V.! 1)) (\col -> (matrix V.! col) VUB.! rowNumber)


