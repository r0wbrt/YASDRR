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
Module      :  YASDRR.SDR.ChirpRadar
Description :  Functions that can be used to build a chirp radar.
Copyright   :  (c) Robert C. Taylor
License     :  Apache 2.0

Maintainer  :  r0wbrt@gmail.com
Stability   :  unstable
Portability :  portable
-}

module YASDRR.SDR.ChirpRadar
    ( ChirpRadarSettings (..)
    , SignalWindow ( HammingWindow
                   , NoWindow
                   )
    , chirpRx
    , chirpTx
    , generateChirp
    ) where

-- System imports
import           Data.Complex
import qualified Data.Vector.Storable   as VST

-- yasdrr library imports
import qualified YASDRR.DSP.Correlation as Cor
import qualified YASDRR.DSP.Windows     as Windows


-- | Type of signal window
data SignalWindow = HammingWindow | NoWindow


-- | Settings controlling the behavior of the chirp modules.
data ChirpRadarSettings = ChirpRadarSettings
 { optStartFrequency        :: Float
 , optEndFrequency          :: Float
 , optFrequencyShift        :: Float
 , optSampleRate            :: Float
 , optRiseTime              :: Float
 , optSilenceLength         :: Int
 , optSilenceTruncateLength :: Int
 , optAmplitude             :: Float
 , optChirpWindow           :: SignalWindow
 }


-- | Takes a ChirpRadarSettings record and then returns a function that
--   can process a chirp radar return pulse.
chirpRx :: ChirpRadarSettings ->
                    (VST.Vector (Complex Float) -> VST.Vector (Complex Float))
chirpRx programSettings = compressReturn windowedChirp
    where windowedChirp = generateChirpUsingSettings programSettings {optAmplitude = 1.0}


-- | Generates a chirp radar pulse using the supplied settings.
chirpTx :: ChirpRadarSettings -> VST.Vector (Complex Float)
chirpTx programSettings = windowedChirp VST.++ VST.replicate silenceLength (0.0 :+ 0.0)
    where windowedChirp = generateChirpUsingSettings programSettings
          silenceLength = optSilenceLength programSettings


-- | Generates a chirp using the supplied ChirpRadarSettings.
generateChirpUsingSettings :: ChirpRadarSettings -> VST.Vector (Complex Float)
generateChirpUsingSettings programSettings = windowedChirp
    where sampleRate = optSampleRate programSettings

          frequecyShift = optFrequencyShift programSettings
          startFrequency = frequecyShift +  optStartFrequency programSettings
          endFrequency = frequecyShift + optEndFrequency programSettings

          amplitude = optAmplitude programSettings

          chirpLength = optRiseTime programSettings


          normalizedChirp = generateChirp sampleRate startFrequency endFrequency chirpLength
          window = getChirpWindow (optChirpWindow programSettings) $ floor chirpLength
          adjustedChirp = VST.map (\sample -> (amplitude :+ 0.0) * sample ) normalizedChirp
          windowedChirp = VST.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window adjustedChirp


-- | Compresses the a returned signal using a reference chirp signal, and a pulseWindow.
compressReturn :: VST.Vector (Complex Float) ->
                   VST.Vector (Complex Float) -> VST.Vector (Complex Float)
compressReturn = Cor.correlate


-- | Generates a chirp window based on the selected signal window.
getChirpWindow :: SignalWindow -> Int -> VST.Vector Float
getChirpWindow window n = case window of
                          HammingWindow -> VST.convert $ Windows.hammingWindowV n
                          NoWindow      -> VST.replicate n 1.0


-- | Generates a linear chirp.
generateChirp :: Float -> Float -> Float -> Float -> VST.Vector (Complex Float)
generateChirp sampleRate startFrequency endFrequency chirpLength = VST.fromList signalList
    where signalList = [cis $ phi $ fromIntegral i | i <- [0::Int .. floor chirpLength] ]
          phi t = ((pi*fd*t*t) / chirpLength) + ((2.0*pi*startFrequency*t) / sampleRate)
          fd = (endFrequency - startFrequency) / sampleRate
