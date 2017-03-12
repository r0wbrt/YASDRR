module Shared.ChirpRx where


import qualified Shared.ChirpCommon as ChirpCommon
import System.Console.GetOpt as GetOpt
import qualified YASDRR.Recipes.ChirpRx as ChirpRx
import qualified Data.ByteString as B
import qualified YASDRR.Recipes.SharedRecipesOptions as SROptions
import qualified Shared.CommandLine as CL
import qualified Data.Vector.Unboxed as VUB
import qualified YASDRR.IO.ComplexSerialization as IOComplex
import Data.Complex

processCommandInput :: GetOpt.ArgOrder (ChirpCommon.ChirpOptions -> IO ChirpCommon.ChirpOptions) -> [String] ->  (IO ChirpCommon.ChirpOptions, [String], [String])
processCommandInput argOrder arguments = (CL.processInput ChirpCommon.startOptions actions, extra, errors)
    where (actions, extra, errors) = GetOpt.getOpt argOrder ChirpCommon.chirpRadarRxOptions arguments 

    
chirpRxMain :: ChirpCommon.ChirpOptions -> IO () 
{-# ANN module "HLint: ignore Use :" #-}
chirpRxMain programSettings = do
              
              let chirpLength = ChirpCommon.calculateSignalLength programSettings
              let outputFormat = ChirpCommon.optOutputSampleFormat programSettings
              let inputFormat = ChirpCommon.optInputSampleFormat programSettings
              
              let silenceLength = ChirpCommon.optSilenceLength programSettings
              
              let pulseTruncationLength = ChirpCommon.optSilenceTruncateLength programSettings
              
              let inputReader = ChirpCommon.optInputReader programSettings
              let signalReader = readInput (floor chirpLength + silenceLength) pulseTruncationLength inputFormat inputReader
               
              let signalWriter signal = ChirpCommon.optOutputWriter programSettings $ ChirpCommon.serializeOutput outputFormat signal
              
              let chirpSettings = SROptions.ChirpRadarSettings
                    { SROptions.optStartFrequency = ChirpCommon.optStartFrequency programSettings
                    , SROptions.optEndFrequency = ChirpCommon.optEndFrequency programSettings
                    , SROptions.optFrequencyShift = ChirpCommon.optFrequencyShift programSettings
                    , SROptions.optSampleRate = ChirpCommon.optSampleRate programSettings
                    , SROptions.optRiseTime = chirpLength
                    , SROptions.optSilenceLength = silenceLength
                    , SROptions.optSilenceTruncateLength = pulseTruncationLength
                    , SROptions.optAmplitude = ChirpCommon.optAmplitude programSettings
                    , SROptions.optChirpWindow = ChirpCommon.optChirpWindow programSettings
                    , SROptions.optSignalWindow = ChirpCommon.optSignalWindow programSettings
                    }
              
              let signalProcessor = ChirpRx.main chirpSettings
              
              processData signalProcessor signalReader signalWriter
              
              
processData :: (VUB.Vector (Complex Double) ->
                VUB.Vector (Complex Double)) -> 
                 IO (Maybe ( VUB.Vector (Complex Double) ) )  -> 
                  (VUB.Vector (Complex Double) -> IO ()) -> IO ()
processData signalProcessor signalReader signalWriter = do
    
    fileInput <- signalReader
    
    case fileInput of
         Just radarReturn -> do
             signalWriter $ signalProcessor radarReturn
             processData signalProcessor signalReader signalWriter
         Nothing -> return ()
         
         
readInput :: Int -> Int -> ChirpCommon.SampleFormat -> (Int -> IO B.ByteString) -> IO (Maybe (VUB.Vector (Complex Double)))
readInput signalLength pulseTruncationLength sampleFormat reader = do    
        
    fileBlock <- reader signalLengthBytes
    
    return $ if B.length fileBlock < signalLengthBytes then Nothing 
                    else Just $ deserializer (B.take (B.length fileBlock - truncationLengthBytes) fileBlock) 
    
    where sampleSize = case sampleFormat of
                        ChirpCommon.SampleComplexDouble -> 16
                        ChirpCommon.SampleComplexFloat -> 8
                        ChirpCommon.SampleComplexSigned16 -> 4
          
          signalLengthBytes = signalLength * sampleSize
          truncationLengthBytes = sampleSize * pulseTruncationLength
          
          deserializer bString = VUB.fromList $ fst $ IOComplex.deserializeBlock decoder bString
            where decoder = case sampleFormat of
                                ChirpCommon.SampleComplexDouble -> IOComplex.complexDoubleDeserializer
                                ChirpCommon.SampleComplexFloat -> IOComplex.complexFloatDeserializer
                                ChirpCommon.SampleComplexSigned16 -> IOComplex.complexSigned16Deserializer 1.0


