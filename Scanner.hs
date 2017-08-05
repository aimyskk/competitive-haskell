module Scanner (
  readInt,
  readInteger,
  readInts,
  readIntegers,
  readIntPair,
  readIntegerPair
) where

import qualified Data.ByteString.Char8 as B

readInt :: B.ByteString -> Int
readInt = maybe undefined fst . B.readInt

readInteger :: B.ByteString -> Integer
readInteger = maybe undefined fst . B.readInteger

readInts :: B.ByteString -> [Int]
readInts = map readInt . B.words

readIntegers :: B.ByteString -> [Integer]
readIntegers = map readInteger . B.words

readIntPair :: B.ByteString -> [(Int,Int)]
readIntPair = map (pair . map readInt . B.words) . B.lines

readIntegerPair :: B.ByteString -> [(Integer,Integer)]
readIntegerPair = map (pair . map readInteger . B.words) . B.lines

pair :: [t] -> (t, t)
pair [x, y] = (x, y)
pair _ = undefined