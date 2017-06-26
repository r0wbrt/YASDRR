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
import Data.Word
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VUB
import Data.List
import Data.Array
import Data.Complex
import qualified Data.ByteString as B
import qualified YASDRR.SDR.MorseCode as Morse

main :: IO ()
main = defaultMain tests

tests = [
         testGroup "Morse Code" [
            testCase "Convert SOS" morseCodeConvertSOSTest,
            testCase "Convert Quick Brown Fox" morseCodeConvertQuickBrownFoxTest
            --TODO
           -- testCase "Encode SOS" morseCodeEncodeSOSTest,
            --testCase "Encode Quick Brown Fox" morseCodeEncodeQuickBrownFoxTest
         ]
        ]

        
morseCodeConvertSOSTest = assertEqual "SOS was not encoded properly by the morse encoder." expectedOutput givenOutput
    where expectedOutput = [Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,
                                Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,
                                    Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot]
          testString ="sOS" -- Morse Sequence: ... --- ...
          givenOutput = Morse.convertStringToMorseCode testString
    

morseCodeConvertQuickBrownFoxTest = assertEqual "The Quick Brown Fox Jumps Over The laZy dOg was not encoded properly by the morse encoder" expectedOutput givenOutput
    where testString = "The Quick  Brown Fox Jumps Over The laZy dOg  " -- Morse Sequence: - .... . / --.- ..- .. -.-. -.- / -... .-. --- .-- -. / ..-. --- -..- / .--- ..- -- .--. ... / --- ...- . .-. / - .... . / .-.. .- --.. -.-- / -.. --- --.
          expectedOutput = [Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot] 
          givenOutput = Morse.convertStringToMorseCode testString
        
compareDouble a b = (abs (a - b)) < 0.005
compareComplex a b = (compareDouble (realPart a) (realPart b)) && (compareDouble (imagPart a) (imagPart b))
