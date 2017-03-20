

module YASDRR.SDR.ChirpRadar (ChirpRadarSettings (..), 
        SignalWindow (HammingWindow, NoWindow), chirpRx, chirpTx, generateChirp) where 


import Data.Complex
import Data.Maybe
import qualified Data.Vector.Unboxed as VUB
import qualified YASDRR.DSP.Correlation as Cor
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


chirpRx :: ChirpRadarSettings -> 
                    (VUB.Vector (Complex Double) -> VUB.Vector (Complex Double))
chirpRx programSettings = compressReturn windowedChirp pulseWindow
    where sampleRate = optSampleRate programSettings
          
          frequecyShift = optFrequencyShift programSettings
          startFrequency = frequecyShift +  optStartFrequency programSettings
          endFrequency = frequecyShift + optEndFrequency programSettings
          
          chirpLength = optRiseTime programSettings
          silenceLength = optSilenceLength programSettings
          
          normalizedChirp = generateChirp sampleRate startFrequency endFrequency chirpLength
          
          window = getChirpWindow (optChirpWindow programSettings) $ floor chirpLength 
          
          windowedChirp = VUB.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window normalizedChirp
          
          pulseTruncationLength = optSilenceTruncateLength programSettings
          
          pulseWindow = getPulseWindow (optSignalWindow programSettings) (floor chirpLength + silenceLength - pulseTruncationLength)


chirpTx :: ChirpRadarSettings -> VUB.Vector (Complex Double)
chirpTx programSettings = windowedChirp VUB.++ VUB.replicate silenceLength (0.0 :+ 0.0)

    where sampleRate = optSampleRate programSettings
          
          frequecyShift = optFrequencyShift programSettings
          startFrequency =frequecyShift +  optStartFrequency programSettings
          endFrequency = frequecyShift + optEndFrequency programSettings
              
          amplitude = optAmplitude programSettings
              
          chirpLength = optRiseTime programSettings
          silenceLength = optSilenceLength programSettings
              
              
          normalizedChirp = generateChirp sampleRate startFrequency endFrequency chirpLength
          window = getChirpWindow (optChirpWindow programSettings) $ floor chirpLength 
          adjustedChirp = VUB.map (\sample -> (amplitude :+ 0.0) * sample ) normalizedChirp
          windowedChirp = VUB.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window adjustedChirp


compressReturn :: VUB.Vector (Complex Double) -> Maybe (VUB.Vector (Complex Double)) -> 
                        VUB.Vector (Complex Double) -> VUB.Vector (Complex Double)
compressReturn chirp pulseWindow signal = Cor.correlateV chirp windowedSignal

    where windowedSignal = if isNothing pulseWindow then 
                            signal 
                                else VUB.zipWith (*) (fromJust pulseWindow) signal


getPulseWindow :: SignalWindow -> Int -> Maybe (VUB.Vector (Complex Double))
getPulseWindow window n = case window of
                          HammingWindow -> Just $ VUB.map (:+ 0) $ Windows.hammingWindowV n
                          NoWindow -> Nothing


getChirpWindow :: SignalWindow -> Int -> VUB.Vector Double
getChirpWindow window n = case window of
                          HammingWindow -> Windows.hammingWindowV n
                          NoWindow -> VUB.replicate n 1.0


generateChirp :: Double -> Double -> Double -> Double -> VUB.Vector (Complex Double)
generateChirp sampleRate startFrequency endFrequency chirpLength = VUB.fromList signalList
    where signalList = [cis $ phi $ fromIntegral i | i <- [0::Int .. floor chirpLength] ]
          phi t = ((pi*fd*t*t) / chirpLength) + ((2.0*pi*startFrequency*t) / sampleRate)
          fd = (endFrequency - startFrequency) / sampleRate
