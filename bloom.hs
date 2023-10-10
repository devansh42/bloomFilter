module Bloom
  ( empty,
    Bloom.elem,
    add,
    BloomFilter,
  )
where

import Data.Binary (Word32, Word8)
import Data.Bits (Bits (shiftL), (.&.), (.|.))
import Data.IntMap (insert)
import Data.Sequence as Sq
  ( Seq (Empty),
    length,
    lookup,
    replicate,
    update,
    (><),
    (|>),
  )
import GHC.Float (int2Double)
import Hash (Hashable (hash32))

type ByteSeq = Sq.Seq Word8

data BloomFilter = BF Int Int ByteSeq -- BF bitArraySize numOfHashes bitArray

instance Show BloomFilter where
  show :: BloomFilter -> String
  show (BF bits hashes seq) = "BF " <> show bits <> " " <> show hashes <> " " <> show seq

defaultSeed :: Word32
defaultSeed = 0x9747b28c

one = 1 :: Word8

zero = 0 :: Word8

-- creates an empty sequence
empty :: Int -> Double -> BloomFilter
empty entries error =
  let log2Square = log 2 ** 2
      bitsPerEntry = -log error / log2Square
      bitArraySize = ceiling $ (* int2Double entries) bitsPerEntry
      numHashes = ceiling $ (* log 2) bitsPerEntry
   in BF bitArraySize numHashes Sq.Empty

bitIndicies :: (Hashable p) => BloomFilter -> p -> [Int]
bitIndicies (BF bits hashes _) entry =
  let hash1 = hash32 entry defaultSeed
      hash2 = hash32 entry hash1
      inthash1 = fromIntegral hash1 :: Int
      inthash2 = fromIntegral hash2 :: Int
      ar = [mod (inthash1 + i * inthash2) bits | i <- [0 .. hashes - 1]]
   in ar

isBitsSet :: ByteSeq -> Int -> Bool
isBitsSet seq index =
  let (byteIndex, bitIndex) = quotRem index 8
      shiftCount = 8 - bitIndex
      matchingByte = shiftL one shiftCount
      matchingFn = (== matchingByte) . (.&.) matchingByte
   in maybe False matchingFn $ Sq.lookup byteIndex seq

areBitsSet :: ByteSeq -> [Int] -> Bool
areBitsSet _ [] = True
areBitsSet bseq (index : indicies) = isBitsSet bseq index && areBitsSet bseq indicies

setBit :: ByteSeq -> Int -> ByteSeq
setBit seq index =
  let (byteIndex, bitIndex) = quotRem index 8
      shiftCount = 8 - bitIndex
      matchingByte = shiftL one shiftCount
      finalByte = (.|.) matchingByte
      tailSeq = Sq.replicate (byteIndex - Sq.length seq) zero
   in case Sq.lookup byteIndex seq of
        Nothing -> (seq Sq.>< tailSeq) Sq.|> matchingByte
        Just byte -> Sq.update byteIndex (finalByte byte) seq

elem :: (Hashable a) => BloomFilter -> a -> Bool
elem bf entry =
  let indicies = bitIndicies bf entry
      (BF _ _ seq) = bf
   in areBitsSet seq indicies

add :: (Hashable a) => BloomFilter -> a -> BloomFilter
add (BF bits hashes seq) entry =
  let bIndicies = bitIndicies (BF bits hashes seq) entry
   in BF bits hashes $ foldl setBit seq bIndicies
