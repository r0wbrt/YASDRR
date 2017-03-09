--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  YASDRR.Recipes.ChirpRx
Description :  Recipe for processing chirp radar returns.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This recipe performs linear chirp radar compression.
-}

module YASDRR.Recipes.ChirpRx (main) where 

import Data.Complex
import Data.Maybe
import qualified YASDRR.Recipes.SharedRecipesOptions as Opts
import qualified Data.Vector.Unboxed as VUB
import qualified YASDRR.SDR.ChirpRadar as Chirp
import qualified YASDRR.DSP.Correlation as Cor
import qualified YASDRR.DSP.Windows as Windows


main :: Opts.ChirpRadarSettings -> 
                    (VUB.Vector (Complex Double) -> VUB.Vector (Complex Double))
{-# ANN module "HLint: ignore Use :" #-}
main programSettings = compressReturn windowedChirp pulseWindow
    where sampleRate = Opts.optSampleRate programSettings
          
          frequecyShift = Opts.optFrequencyShift programSettings
          startFrequency = frequecyShift +  Opts.optStartFrequency programSettings
          endFrequency = frequecyShift + Opts.optEndFrequency programSettings
          
          chirpLength = Opts.optRiseTime programSettings
          silenceLength = Opts.optSilenceLength programSettings
          
          normalizedChirp = Chirp.generateChirp sampleRate startFrequency endFrequency chirpLength
          
          window = Opts.getChirpWindow (Opts.optChirpWindow programSettings) $ floor chirpLength 
          
          windowedChirp = VUB.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window normalizedChirp
          
          pulseTruncationLength = Opts.optSilenceTruncateLength programSettings
          
          pulseWindow = getPulseWindow (Opts.optSignalWindow programSettings) (floor chirpLength + silenceLength - pulseTruncationLength)
          
          
compressReturn :: VUB.Vector (Complex Double) -> Maybe (VUB.Vector (Complex Double)) -> 
                        VUB.Vector (Complex Double) -> VUB.Vector (Complex Double)
compressReturn chirp pulseWindow signal = Cor.correlateV chirp windowedSignal

    where windowedSignal = if isNothing pulseWindow then 
                            signal 
                                else VUB.zipWith (*) (fromJust pulseWindow) signal
                                
                                
getPulseWindow :: Opts.SignalWindow -> Int -> Maybe (VUB.Vector (Complex Double))
getPulseWindow window n = case window of
                          Opts.HammingWindow -> Just $ VUB.map (:+ 0) $ Windows.hammingWindowV n
                          Opts.NoWindow -> Nothing
                          
                          
