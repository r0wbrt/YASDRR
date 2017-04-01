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
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VUB

--Converts a value to the nearest value of 2 assuming the value is a power of 2.
discretePowerOf2 :: Int -> Int
discretePowerOf2 number
    | setBit 0 lowerValue < number  = lowerValue + 1
    | otherwise                     = lowerValue
    where lowerValue = (finiteBitSize number - 1) - countLeadingZeros number

-- | Grabs a row from a vector based matrix.
getVectorMatrixRow :: (VUB.Unbox a) => Int -> VB.Vector (VUB.Vector a) -> VUB.Vector a
getVectorMatrixRow rowNumber matrix =  VUB.convert $ VB.map (VUB.! rowNumber) matrix


