--Copyright Robert C. Taylor

module OFDMRadar.Math.Misc where

import Data.Bits
import qualified Data.Vector as V

--Converts a value to the nearest value of 2 assuming the value is a power of 2.
discretePowerOf2 :: Int -> Int
discretePowerOf2 number = if (setBit 0 lowerValue) < number then lowerValue + 1 else lowerValue
    where lowerValue = (finiteBitSize number - 1) - countLeadingZeros number
 
 
getVectorMatrixRow rowNumber vectorList = V.map (\vector -> vector V.! rowNumber) vectorList


