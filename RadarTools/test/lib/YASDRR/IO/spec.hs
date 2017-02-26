--Copyright Robert C. Taylor

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Word
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString as B
import YASDRR.IO.ComplexSerialization

main :: IO ()
main = defaultMain tests


tests = [ testGroup "Deserialize block test" [
              testProperty "Deserialize Block Quick Check" deserializeBlockQuickCheck
            ],
          testGroup "Serialize block test" [
              testProperty "Serialize Block Quick Check" serializeBlockQuickCheck
            ],
          testGroup "Serialize block list test" [
              testProperty "Serialize Block List Quick Check" serializeBlockListQuickCheck
            ],
          testGroup "Deserialize block list test" [
              testProperty "Deserialize Block List Quick Check" deserializeBlockListQuickCheck
            ]
       ]

serializeBlockQuickCheck :: [Word8] -> Bool 
serializeBlockQuickCheck input = (B.unpack $ serializeBlock word8testEncoder input) == input

deserializeBlockQuickCheck :: [Word8] -> Bool 
deserializeBlockQuickCheck input = (fst $ deserializeBlock word8testDecoder (B.pack input)) == input

serializeBlockListQuickCheck :: [Word8] -> Int -> Bool
serializeBlockListQuickCheck input times = B.unpack (serializeBlock (blockListSerializer word8testEncoder) extInput) == (concat extInput)
    where adjTimes = abs times
          extInput = replicate adjTimes input

deserializeBlockListQuickCheck :: [Word8] -> Int -> Bool
deserializeBlockListQuickCheck input times = (fst $ deserializeBlock (blockListDeserializer word8testDecoder (length input)) (B.pack $ concat inputAdj)) == (inputCheck)
    where inputAdj = replicate adjTimes input
          inputCheck = if input == [] then [] else inputAdj
          adjTimes = abs times
          
word8testEncoder byte = BP.putWord8 byte

word8testDecoder = do
    value <- BG.getWord8
    return value
