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

module YASDRR.SDR.ChirpRadar (ChirpRadarSettings (..),
        SignalWindow (HammingWindow, NoWindow), chirpRx, chirpTx, generateChirp) where

-- System imports
import           Data.Complex
import           Data.Maybe
import qualified Data.Vector.Unboxed    as VUB

-- yasdrr library imports
import qualified YASDRR.DSP.Correlation as Cor
import qualified YASDRR.DSP.Windows     as Windows


-- | Type of signal window
data SignalWindow = HammingWindow | NoWindow


-- | Settings controlling the behavior of the chirp modules.
data ChirpRadarSettings = ChirpRadarSettings
 { optStartFrequency        :: Double
 , optEndFrequency          :: Double
 , optFrequencyShift        :: Double
 , optSampleRate            :: Double
 , optRiseTime              :: Double
 , optSilenceLength         :: Int
 , optSilenceTruncateLength :: Int
 , optAmplitude             :: Double
 , optChirpWindow           :: SignalWindow
 , optSignalWindow          :: SignalWindow
 }


-- | Takes a ChirpRadarSettings record and then returns a function that
--   can process a chirp radar return pulse.
chirpRx :: ChirpRadarSettings ->
                    (VUB.Vector (Complex Double) -> VUB.Vector (Complex Double))
chirpRx programSettings = compressReturn windowedChirp pulseWindow
    where chirpLength = optRiseTime programSettings
          silenceLength = optSilenceLength programSettings

          windowedChirp = generateChirpUsingSettings programSettings {optAmplitude = 1.0}

          pulseTruncationLength = optSilenceTruncateLength programSettings

          pulseWindow = getPulseWindow (optSignalWindow programSettings) (floor chirpLength + silenceLength - pulseTruncationLength)


-- | Generates a chirp radar pulse using the supplied settings.
chirpTx :: ChirpRadarSettings -> VUB.Vector (Complex Double)
chirpTx programSettings = windowedChirp VUB.++ VUB.replicate silenceLength (0.0 :+ 0.0)
    where windowedChirp = generateChirpUsingSettings programSettings
          silenceLength = optSilenceLength programSettings


-- | Generates a chirp using the supplied ChirpRadarSettings.
generateChirpUsingSettings :: ChirpRadarSettings -> VUB.Vector (Complex Double)
generateChirpUsingSettings programSettings = windowedChirp
    where sampleRate = optSampleRate programSettings

          frequecyShift = optFrequencyShift programSettings
          startFrequency = frequecyShift +  optStartFrequency programSettings
          endFrequency = frequecyShift + optEndFrequency programSettings

          amplitude = optAmplitude programSettings

          chirpLength = optRiseTime programSettings


          normalizedChirp = generateChirp sampleRate startFrequency endFrequency chirpLength
          window = getChirpWindow (optChirpWindow programSettings) $ floor chirpLength
          adjustedChirp = VUB.map (\sample -> (amplitude :+ 0.0) * sample ) normalizedChirp
          windowedChirp = VUB.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window adjustedChirp


-- | Compresses the a returned signal using a reference chirp signal, and a pulseWindow.
compressReturn :: VUB.Vector (Complex Double) -> Maybe (VUB.Vector (Complex Double)) ->
                        VUB.Vector (Complex Double) -> VUB.Vector (Complex Double)
compressReturn chirp pulseWindow signal = Cor.correlateFFIV chirp windowedSignal

    where windowedSignal = if isNothing pulseWindow then
                            signal
                                else VUB.zipWith (*) (fromJust pulseWindow) signal


-- | Generates a pulse window based on the selected SignalWindow
getPulseWindow :: SignalWindow -> Int -> Maybe (VUB.Vector (Complex Double))
getPulseWindow window n = case window of
                          HammingWindow -> Just $ VUB.map (:+ 0) $ Windows.hammingWindowV n
                          NoWindow -> Nothing


-- | Generates a chirp window based on the selected signal window.
getChirpWindow :: SignalWindow -> Int -> VUB.Vector Double
getChirpWindow window n = case window of
                          HammingWindow -> Windows.hammingWindowV n
                          NoWindow      -> VUB.replicate n 1.0


-- | Generates a linear chirp.
generateChirp :: Double -> Double -> Double -> Double -> VUB.Vector (Complex Double)
generateChirp sampleRate startFrequency endFrequency chirpLength = VUB.fromList signalList
    where signalList = [cis $ phi $ fromIntegral i | i <- [0::Int .. floor chirpLength] ]
          phi t = ((pi*fd*t*t) / chirpLength) + ((2.0*pi*startFrequency*t) / sampleRate)
          fd = (endFrequency - startFrequency) / sampleRate
