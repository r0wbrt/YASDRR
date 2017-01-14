--Copyright Robert C. Taylor


module OFDMRadar.SDR.Converters (bytesToSymbols) where

import qualified Data.ByteString as B
import qualified Data.Array as Array
import Data.Word
import Data.Bits
import qualified OFDMRadar.Math.Misc as MathMisc

-- | Converts a data block into a list of symbols. Symbols are in LIFO order and have little edian bit order.
bytesToSymbols wordSize constellationArray dataBlock = 
    
    if dataBlock == B.empty || isWordSizePowerOf2AndSizeMost8 then
        [] 
    else 
        ((bytesToSymbols wordSize constellationArray (B.tail dataBlock)) ++ (reverse $ (byteToSymbol constellationArray wordSize 0 (B.head dataBlock))))
        
    where isWordSizePowerOf2AndSizeMost8 = if (2 ^ MathMisc.discretePowerOf2 wordSize) == wordSize && wordSize <= 8 then False else error "bytesToSymbols was supplied with invalid word size"
    
          
          
byteToSymbol _ _ 8 _ = []
byteToSymbol constellationArray wordSize shiftC byte = (constellationArray Array.! index):(byteToSymbol constellationArray wordSize (w8wordSize + shiftC) byte)

    --Subtract one to get the bit mask. Assumes wordsize is a power of 2. Eg 2,4,8,16....
    where index = (shiftR byte shiftC) .&. (setBits (wordSize - 1) 0)
          setBits (-1) byte = byte
          setBits n byte = setBits (n - 1) (setBit byte n)  
          w8wordSize = fromIntegral . toInteger $ wordSize --Needed for array indexing
