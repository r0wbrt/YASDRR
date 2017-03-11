--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  YASDRR.Recipes.ChirpTx
Description :  Recipe for generating chirp radar pulses.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

This recipe generates linear chirps pulses for chirp based pulse compression radar.
-}

module YASDRR.Recipes.ChirpTx (main) where

import qualified YASDRR.SDR.ChirpRadar as Chirp
import qualified YASDRR.Recipes.SharedRecipesOptions as Opts
import qualified Data.Vector.Unboxed as VUB
import Data.Complex


main :: Opts.ChirpRadarSettings -> VUB.Vector (Complex Double)

{-# ANN module "HLint: ignore Use :" #-}
main programSettings = windowedChirp VUB.++ VUB.replicate silenceLength (0.0 :+ 0.0)

    where sampleRate = Opts.optSampleRate programSettings
          
          frequecyShift = Opts.optFrequencyShift programSettings
          startFrequency =frequecyShift +  Opts.optStartFrequency programSettings
          endFrequency = frequecyShift + Opts.optEndFrequency programSettings
              
          amplitude = Opts.optAmplitude programSettings
              
          chirpLength = Opts.optRiseTime programSettings
          silenceLength = Opts.optSilenceLength programSettings
              
              
          normalizedChirp = Chirp.generateChirp sampleRate startFrequency endFrequency chirpLength
          window = Opts.getChirpWindow (Opts.optChirpWindow programSettings) $ floor chirpLength 
          adjustedChirp = VUB.map (\sample -> (amplitude :+ 0.0) * sample ) normalizedChirp
          windowedChirp = VUB.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window adjustedChirp
