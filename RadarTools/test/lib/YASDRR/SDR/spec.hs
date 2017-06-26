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


import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import qualified YASDRR.SDR.MorseCode           as Morse

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [
         testGroup "Morse Code" [
            testCase "Convert SOS" morseCodeConvertSOSTest,
            testCase "Convert Quick Brown Fox" morseCodeConvertQuickBrownFoxTest
            --TODO
           -- testCase "Encode SOS" morseCodeEncodeSOSTest,
            --testCase "Encode Quick Brown Fox" morseCodeEncodeQuickBrownFoxTest
         ]
        ]



morseCodeConvertSOSTest :: Assertion
morseCodeConvertSOSTest = assertEqual "SOS was not encoded properly by the morse encoder." expectedOutput givenOutput
    where expectedOutput = [Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,
                                Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,
                                    Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot]
          testString ="sOS" -- Morse Sequence: ... --- ...
          givenOutput = Morse.convertStringToMorseCode testString


morseCodeConvertQuickBrownFoxTest :: Assertion
morseCodeConvertQuickBrownFoxTest = assertEqual "The Quick Brown Fox Jumps Over The laZy dOg was not encoded properly by the morse encoder" expectedOutput givenOutput
    where testString = "The Quick  Brown Fox Jumps Over The laZy dOg  " -- Morse Sequence: - .... . / --.- ..- .. -.-. -.- / -... .-. --- .-- -. / ..-. --- -..- / .--- ..- -- .--. ... / --- ...- . .-. / - .... . / .-.. .- --.. -.-- / -.. --- --.
          expectedOutput = [Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace,Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseDot, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDash, Morse.MorseSpace, Morse.MorseDot]
          givenOutput = Morse.convertStringToMorseCode testString

