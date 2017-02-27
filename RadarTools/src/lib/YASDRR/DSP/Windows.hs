--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  YASDRR.DSP.Windows
Description :  Window generation functions.
               sequences of complex numbers.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

Functionality to generate signal widnows. 
-}


module YASDRR.DSP.Windows where

import qualified Data.Vector as V


hammingWindowV :: Int -> V.Vector Double
hammingWindowV n = V.fromList [0.54 - 0.46 * cos((2.0 * pi * fromIntegral i) / fromIntegral (n - 1)) | i <- [0..n-1]]
