-- Copyright Robert C. Taylor, All Rights Reserved


module OFDMRadar.SDR.OFDMModulation where

import qualified OFDMRadar.SDR.Converters as SDRConverters
import qualified OFDMRadar.DSP.FFT as DSPFft
import qualified OFDMRadar.SDR.DopplerRadar as DopplerRadar
import qualified OFDMRadar.DSP.Correlation as Correlation
import qualified OFDMRadar.Math.Misc as MathMisc
import qualified Data.ByteString as B

import Data.Complex
import qualified Data.Vector as V

-- | extends an OFDM symbol by applying silence and cyclic prefix to it.
extendOFDMSymbol _ _ [] = []
extendOFDMSymbol cyclicLength lengthOfSilence symbol =  prefix ++ symbol ++ silence

    where prefix = (reverse $ take cyclicLength $ cycle $ reverse symbol)
          
          silence = (replicate lengthOfSilence (0 :+ 0))
    
    
-- | Encodes a data block into an OFDM Symbol
encodeOFDMSymbol 0 _ _ _ = []
encodeOFDMSymbol _ _ 0 _ = []

encodeOFDMSymbol symbolSize symbolMapping carrierCount dataBlock = 
    
    --Convert the data only if it is valid.
    if carrierIsPowerOf2 && inputDataBlockValid then
    
        ifft symbols 
        
                  else error $ "Number of symbols provided did not match the number of desired carriers or number of carriers was not a power of 2 for encodeOFDMSymbol."
    
    --Set up inverse FFT
    where ifft = DSPFft.createFft carrierCount (1)
          
          --Check to make sure the number of provided symbols match the carrierCount
          numberOfSymbols = div ((B.length dataBlock)*8) symbolSize
          
          --Carrier count should be a power of 2.
          --Number of carriers should at least be greater then 8
          carrierIsPowerOf2 = if (2 ^ MathMisc.discretePowerOf2 carrierCount) == carrierCount && carrierCount*symbolSize >=8 then True else (error "Number of carriers is not a power of 2 or symbols * carriers is less then 8")
          
          --Conditions to check.
          inputDataBlockValid = if (numberOfSymbols == carrierCount) then True else (error "Number of symbols does not match the number of carriers")
    
          --To Symbols creates a stream of symbols where the first symbol is at the end of the list. 
          --Reverse so the first symbol of the first byte is at the head of the list
          symbols = reverse $ SDRConverters.bytesToSymbols symbolSize symbolMapping dataBlock

-- | Demodulates an OFDM Radar return. 
processOfdmRadarReturnV impulse shift pulses = DopplerRadar.processDopplerReturnV $ V.map (Correlation.correlateV impulse) $ (DSPFft.cyclicMutateMatrixV shift (V.length $ pulses V.! 0) pulses)
