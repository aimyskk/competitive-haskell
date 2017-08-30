module Scanner where

import qualified Data.ByteString.Char8 as B

readInt :: B.ByteString -> Int
readInt = maybe undefined fst . B.readInt

readInts :: B.ByteString -> [Int]
readInts = map readInt . B.words

readIntTuples :: B.ByteString -> [(Int, Int)]
readIntTuples = map ((\[x,y] -> (x,y)) . map readInt . B.words) . B.lines

readInteger :: B.ByteString -> Integer
readInteger = maybe undefined fst . B.readInteger

readIntegers :: B.ByteString -> [Integer]
readIntegers = map readInteger . B.words

readIntegerTuples :: B.ByteString -> [(Integer, Integer)]
readIntegerTuples = map ((\[x,y] -> (x,y)) . map readInteger . B.words) . B.lines
