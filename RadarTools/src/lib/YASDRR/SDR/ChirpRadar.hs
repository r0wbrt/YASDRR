


module YASDRR.SDR.ChirpRadar where 

import Data.Complex
import qualified Data.Vector as V

generateChirp :: Double -> Double -> Double -> Double -> V.Vector (Complex Double)
generateChirp sampleRate startFrequency endFrequency chirpLength = V.fromList signalList
    where signalList = [cis $ phi $ fromIntegral i | i <- [0::Int .. floor chirpLength] ]
          phi t = ((pi*fd*t*t) / chirpLength) + ((2.0*pi*startFrequency*t) / sampleRate)
          fd = (endFrequency - startFrequency) / sampleRate

