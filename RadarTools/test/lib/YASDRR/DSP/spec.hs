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


import           Data.Complex
import           Data.List
import qualified Data.Vector.Storable                 as VST
import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           YASDRR.DSP.Correlation


maxFloatEpsilon :: Float
maxFloatEpsilon = 0.005

maxPercentError :: Float
maxPercentError = 1.0


compareComplexFloat :: Complex Float -> Complex Float -> Bool
compareComplexFloat (real1 :+ imag1) (real2 :+ imag2) = compareFloat real1 real2 && compareFloat imag1 imag2

compareFloat :: Float -> Float -> Bool
compareFloat ref test
    | abs ref < 1 = abs (ref - test) < maxFloatEpsilon
    | otherwise = ( (test - ref) / ref ) < maxPercentError


correlationKernelTestProp :: [(Float, Float)] -> [(Float, Float)] -> Bool
correlationKernelTestProp in1 in2 = if null in1 then
                                        null actualOutput
                                                  else and $ zipWith compareComplexFloat expectedOutput actualOutput

    where actualOutput = VST.toList $ correlate (VST.fromList impulse) (VST.fromList signal)

          expectedOutput = map (sum . zipWith (*) (map conjugate impulse)) (init $ tails signal)

          signal = map (uncurry (:+)) in1
          impulse = map (uncurry (:+)) in2

tests :: [Test]
tests = [
          testGroup "Correlation" [
           testProperty "Random Correlation test" correlationKernelTestProp
           ]
        ]

main :: IO ()
main = defaultMain tests


