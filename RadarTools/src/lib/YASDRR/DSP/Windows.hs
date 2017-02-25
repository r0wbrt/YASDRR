

module YASDRR.DSP.Windows where

import qualified Data.Vector as V


hammingWindowV :: Int -> V.Vector Double
hammingWindowV n = V.fromList $ [0.54 - 0.46 * cos((2.0 * pi * fromIntegral i) / (fromIntegral (n - 1))) | i <- [0..n-1]]
