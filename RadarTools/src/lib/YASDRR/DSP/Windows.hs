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


module YASDRR.DSP.Windows
(
 hammingWindowV
)
    where

import qualified Data.Vector.Unboxed as VUB

-- | Generates a hamming window
hammingWindowV :: Int -> VUB.Vector Double
hammingWindowV n = VUB.fromList [0.54 - 0.46 * cos((2.0 * pi * fromIntegral i) / fromIntegral (n - 1)) | i <- [0..n-1]]
