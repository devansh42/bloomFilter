module Hash (Hashable (hash32)) where

import Data.Bits (Bits (shiftL, shiftR, xor), (.|.))
import Data.ByteString (unpack)
import Data.ByteString.Char8 (pack)
import Data.Word (Word32, Word64, Word8)

multiplier :: Word32 -> Word32
multiplier = (* 0x5bd1e995)

rotations :: Word32 -> Word32
rotations = flip shiftR 24

zero32 :: Word32
zero32 = 0

zero64 :: Word64
zero64 = 0

class Hashable a where
  hash32 :: a -> Word32 -> Word32

converter :: (Bits a) => a -> a -> a
converter val res =
  let shiftedVal = shiftL res 8
   in (.|.) shiftedVal val

converter32 :: Word8 -> Word32 -> Word32
converter32 a b = converter (fromIntegral a) (fromIntegral b)

toWord32 :: [Word8] -> Word32
toWord32 = foldr converter32 zero32

instance Hashable [Word8] where
  hash32 :: [Word8] -> Word32 -> Word32
  hash32 = hashWord32'

instance Hashable String where
  hash32 :: String -> Word32 -> Word32
  hash32 str = hashWord32' $ unpack . pack $ str

hashWord32' :: [Word8] -> Word32 -> Word32
hashWord32' words seed = hashWord32 words $ xor seed . fromIntegral . length $ words

hashWord32 :: [Word8] -> Word32 -> Word32
hashWord32 [] partialResults =
  let intermediateVal = multiplier . xor partialResults . flip shiftR 13 $ partialResults
   in xor intermediateVal . flip shiftR 15 $ intermediateVal
hashWord32 val partialResult
  | length val < 4 =
      hashWord32 [] $ multiplier . foldr fn partialResult $ zip val [x * 8 | x <- [0 ..]]
  | otherwise = hashWord32 (drop 4 val) result
  where
    afterMultipl = multiplier . toWord32 . take 4 $ val
    calcBytes = multiplier . xor afterMultipl . rotations $ afterMultipl
    result = xor calcBytes . multiplier $ partialResult
    fn (a, b) c = xor c . flip shiftL b $ fromIntegral a