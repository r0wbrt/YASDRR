--Copyright Robert C. Taylor - All Rights Reserved

module OFDMRadar.DSP.FFT (createFft, createFftV, cyclicShiftV, cyclicShift, cyclicMutateMatrix, cyclicMutateMatrixV) where

import qualified Data.Vector as V
import Data.Complex
import Data.Typeable
import Foreign.C.Types 
import Foreign.Ptr
import Foreign.Marshal.Array 
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Foreign.Storable

{-# LANGUAGE ForeignFunctionInterface #-}
foreign import ccall unsafe "fft" c_fft :: Ptr (Complex Double) -> Ptr Double -> Ptr Int-> Int -> Int -> IO ()
foreign import ccall unsafe "cdftOptInit" c_MakeFft :: Int -> Ptr Int -> Ptr Double -> IO ()
         
-- | Performs an fft on the input data in the form of a list.
{-# NOINLINE fft #-}
fft fftSize direction input = unsafeDupablePerformIO $ do
    
    -- Convert the input into a C array
    withArray (inputProper) $ \cinput -> do
        
        -- Convert the pre-computed vector coefficients into a c array
        allocaBytes ((sizeOf (undefined::Double)) * (div fftSize 2)) $ \ccoefs ->
        
                -- Convert the pre-computed IP coefficients into a c array
                allocaBytes ((sizeOf (undefined::Int)) *  ceiling (2.0 + sqrt(fromIntegral fftSize))) $ \cworkarea -> do
                    
                    --perform the fft using the c function
                    c_fft cinput ccoefs cworkarea direction fftSize
                    
                    -- Get the result
                    list <- peekArray fftSize cinput
                    
                    --Return it to the caller.
                    return $ list
                    
                        --Extend the input as needed. The caller should be providing input with the the proper size of the fft.
    where inputProper = if length input > fftSize then
                            take fftSize input 
                                else input ++ (replicate (fftSize - length input) (0 :+ 0) )
           
           
-- | Sets up a list based FFT.
{-# NOINLINE createFft #-}
createFft 0 _ = (\a -> [])
createFft 1 _ = (\a -> take 1 a)
createFft fftSize direction = createFftI fftSize direction (fft)


-- | Sets up a vector based FFT
{-# NOINLINE createFftV #-}
createFftV 0 _ = (\a -> V.empty)
createFftV 1 _ = (\a -> V.take 1 a)
createFftV fftSize direction = createFftI fftSize direction (\x y z -> V.fromList $ fft x y (V.toList z))


-- | Sets up the values to be used by the FFT functions
{-# NOINLINE createFftI #-}
createFftI fftSize direction fftFunc = fftFunc fftSize direction
                        

-- | Cyclic shifts a spectrum by a given number of FFT bins.
cyclicShift shift size signal = zipWith (*) signal cyclicCoefs

    where cyclicCoefs = cycle $ [ cyclicCoef shift size i | i <- [0..(size - 1) ] ]
    
-- | Cyclic shifts a vector spectrum by a given number of FFT bins.
cyclicShiftV shift size signal = V.zipWith (*) signal cyclicCoefs
    
    where cyclicCoefs = V.generate (V.length signal) (\i -> cyclicCoef shift size (mod i size))
    

-- | Calculates the coefs to cyclic mutate a matrix
cyclicCoef shift size pos = cis $ (top) / (bottom)

    where top = -2.0 * pi * shift * fromInteger(fromIntegral pos)
          
          bottom = fromIntegral(size)
          
          
-- | Cyclic sjifts a matrix by a given shift
cyclicMutateMatrixV shift size matrix = V.map multiplyColumn matrix

    --Multiply each column entry with the cyclic shift coef
    where multiplyColumn column = cyclicShiftV shift size column
          

-- | Cyclic shifts a matrix by a given shift
cyclicMutateMatrix shift size matrix = map multiplyColumn matrix

    --Multiply each column entry with the cyclic shift coef
    where multiplyColumn column = cyclicShift shift size column
          
 
