--Copyright Robert C. Taylor

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Word
import YASDRR.Math.Misc
import qualified Data.Vector as V
import Data.List

tests = [ testGroup "Discrete Power of 2 function test" [
           testCase "Value of 1 Test" discretePowerOf2Test_1,
           testCase "Value of 2 Test" discretePowerOf2Test_2,
           testCase "Value of 4 Test" discretePowerOf2Test_4,
           testCase "Value of 8 Test" discretePowerOf2Test_8,
           testCase "Value of 16 Test" discretePowerOf2Test_16,
           testCase "Value of 32 Test" discretePowerOf2Test_32,
           testCase "Value of 64 Test" discretePowerOf2Test_64,
           testCase "Value of 5 Test" discretePowerOf2Test_5,
           testProperty "discretePowerOf2 should always ceil the result." discretePowerOf2RandomCheck_prop
           ],
          testGroup "Vector matrix operations" [
           testProperty "Transpose test" columnCheckTest_prop
          ]
       ]
               
               
discretePowerOf2Test_1 = assertEqual "One should have a power of 0"  0 (discretePowerOf2 1)

discretePowerOf2Test_2 =  assertEqual "Two should have a power of 1"  1 (discretePowerOf2 2)

discretePowerOf2Test_4 =  assertEqual "Four should have a power of 2"  2 (discretePowerOf2 4)

discretePowerOf2Test_8 =  assertEqual "Eight should have a power of 3"  3 (discretePowerOf2 8)

discretePowerOf2Test_16 = assertEqual "Sixteen should have a power of 4"  4 (discretePowerOf2 16)

discretePowerOf2Test_32 = assertEqual "Thirty Two should have a power of 5"  5 (discretePowerOf2 32)

discretePowerOf2Test_64 = assertEqual "Sixity Four should have a power of 6"  6 (discretePowerOf2 64)

discretePowerOf2Test_5 = assertEqual "Five should have a power of 3"  3 (discretePowerOf2 5)

discretePowerOf2RandomCheck_prop :: Int -> Bool
discretePowerOf2RandomCheck_prop inValue = ceiling (log (fromIntegral value) / log(2.0)) == discretePowerOf2 value
    where absValue = if inValue < 0 then (inValue * (-1)) else inValue
          value = if absValue == 0 then 1 else absValue
          
columnCheckTest_prop :: [[Int]] -> Bool
columnCheckTest_prop dataIn = if []==dataIn then True else outputList == expectedList
    where rows = minimum $ (map (length)) dataIn
          dataV = V.fromList $ map (V.fromList) dataIn
          expectedList = transpose $ map (take rows) dataIn
          outputList = V.toList $ V.map (V.toList) $ V.generate rows (\a -> getVectorMatrixRow a dataV)
          
          
main :: IO ()
main = defaultMain tests
