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


import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Array
import Data.Complex
import Data.Word
import System.IO.Unsafe
import YASDRR.DSP.Correlation
import qualified Data.Vector.Unboxed as VUB
import qualified Data.Vector as VB
import qualified Data.Vector.Storable as VST
import Data.List


maxFloatEpsilon :: Float
maxFloatEpsilon = 0.005

maxPercentError :: Float
maxPercentError = 1.0

compareComplexFloat (real1 :+ imag1) (real2 :+ imag2) = compareFloat real1 real2 && compareFloat imag1 imag2

compareFloat ref test
    | abs(ref) < 1 = abs (ref - test) < maxFloatEpsilon
    | otherwise = ( (test - ref) / ref ) < maxPercentError


correlationKernelTest_prop :: [(Float, Float)] -> [(Float, Float)] -> Bool
correlationKernelTest_prop in1 in2 = if in1 == [] then
                                        actualOutput == [] 
                                                  else all (id) $ zipWith compareComplexFloat (expectedOutput) (actualOutput)
                                                  
    where actualOutput = VST.toList $ correlate (VST.fromList impulse) (VST.fromList signal)
          
          expectedOutput = map (\a -> sum $ zipWith (*) (map (conjugate) impulse) a) (init $ tails signal)
          
          signal = map (\(a, b) -> (a :+ b)) in1
          impulse = map (\(a, b) -> (a :+ b)) in2
          
tests = [ 
          testGroup "Correlation" [
           testProperty "Random Correlation test" correlationKernelTest_prop
           ]
        ]

main :: IO ()
main = defaultMain tests


