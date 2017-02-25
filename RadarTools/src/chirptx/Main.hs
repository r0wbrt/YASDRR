


import qualified Options as Opts
import System.Environment
import System.Console.GetOpt as GetOpt
import System.IO
import System.Exit
import qualified YASDRR.SDR.ChirpRadar as Chirp
import qualified YASDRR.DSP.Windows as Windows
import qualified Data.Vector as V
import Data.Complex
import qualified Data.ByteString as B
import qualified YASDRR.IO.ComplexSerialization as IOComplex


main :: IO ()
main = do
    arguments <- getArgs
    case GetOpt.getOpt GetOpt.RequireOrder Opts.chirpRadarTxOptions arguments of
        (actions, [], []) -> do
              programSettings <- Opts.processInput actions
              
              hSetBinaryMode stdout True 
              
              let sampleRate = Opts.optSampleRate programSettings
              let startFrequency = Opts.optStartFrequency programSettings
              let endFrequency = Opts.optEndFrequency programSettings
              let chirpLength = calculateSignalLength programSettings
              let amplitude = Opts.optAmplitude programSettings
              let repetitions = Opts.optRepetitions programSettings
              let outputFormat = Opts.optSampleFormat programSettings
              let silenceLength = Opts.optSilenceLength programSettings
              
              let normalizedChirp = Chirp.generateChirp sampleRate startFrequency endFrequency chirpLength
              let window = getWindow (Opts.optWindow programSettings) $ floor chirpLength 
              let adjustedChirp = V.map (\sample -> (amplitude :+ 0.0) * sample ) normalizedChirp
              let windowedChirp = V.zipWith (\windowCoef sample -> (windowCoef :+ 0) * sample) window adjustedChirp
              
              let finalSignal = serializeOutput outputFormat $ windowedChirp V.++ V.replicate silenceLength (0.0 :+ 0.0)
              
              let writer = Opts.optOutputWriter programSettings
              
              _ <- writeOutput writer finalSignal repetitions
              
              _ <- Opts.optCloseOutput programSettings
              
              exitSuccess
        (_, _, errors) -> do
              hPutStrLn stderr $ unlines $ ["Invalid input supplied"] ++ errors
              exitFailure

              
              
calculateSignalLength :: Opts.ProgramOptions -> Double
calculateSignalLength settings = case Opts.optRiseUnit settings of
                                      Opts.RiseUnitsSeconds -> rate * input
                                      Opts.RiseUnitsSamples -> input
    where input = Opts.optRiseTime settings
          rate = Opts.optSampleRate settings
              
              
writeOutput :: (B.ByteString -> IO ()) -> B.ByteString -> Int -> IO ()
writeOutput _ _ 0 = return ()

writeOutput writer signal count = do
    
    let newCount = if count /= -1 then (count - 1) else -1
    
    writer signal
    writeOutput writer signal newCount
    
    
getWindow :: Opts.SignalWindow -> Int -> V.Vector (Double)
getWindow window n = case window of
                          Opts.HammingWindow -> Windows.hammingWindowV n
                          Opts.NoWindow -> V.replicate n 1.0
    
    
serializeOutput :: Opts.SampleFormat -> V.Vector (Complex Double) -> B.ByteString
serializeOutput format signal = case format of
                                   Opts.SampleComplexDouble -> IOComplex.serializeBlockV IOComplex.complexDoubleSerializer signal
                                   Opts.SampleComplexFloat -> IOComplex.serializeBlockV IOComplex.complexFloatSerializer signal
                                   Opts.SampleComplexSigned16 -> IOComplex.serializeBlockV (IOComplex.complexSigned16Serializer 1.0) signal
