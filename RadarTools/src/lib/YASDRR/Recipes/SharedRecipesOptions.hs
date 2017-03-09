--Copyright Robert C. Taylor - All Rights Reserved

{- |
Module      :  YASDRR.Recipes.SharedRecipesOptions
Description :  Common functionality shared between the chirp recipes.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable 
Portability :  portable 

-}


module YASDRR.Recipes.SharedRecipesOptions where

import qualified Data.Vector.Unboxed as VUB
import qualified YASDRR.DSP.Windows as Windows


data SignalWindow = HammingWindow | NoWindow

data ChirpRadarSettings = ChirpRadarSettings
 { optStartFrequency :: Double
 , optEndFrequency :: Double
 , optFrequencyShift :: Double
 , optSampleRate :: Double
 , optRiseTime :: Double
 , optSilenceLength :: Int
 , optSilenceTruncateLength :: Int
 , optAmplitude :: Double
 , optChirpWindow :: SignalWindow
 , optSignalWindow :: SignalWindow
 } 

getChirpWindow :: SignalWindow -> Int -> VUB.Vector Double
getChirpWindow window n = case window of
                          HammingWindow -> Windows.hammingWindowV n
                          NoWindow -> VUB.replicate n 1.0
