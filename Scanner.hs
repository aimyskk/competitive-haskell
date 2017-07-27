module Scanner (
  readInts,
  readIntegers,
  readIntPair,
  readIntegerPair
) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe

readInts :: IO [Int]
readInts = map readInt . B.words <$> B.getLine

readIntegers :: IO [Integer]
readIntegers = map readInteger . B.words <$> B.getLine

readIntPair :: IO [(Int,Int)]
readIntPair = map (pair . map readInt . B.words) . B.lines <$> B.getContents

readIntegerPair :: IO [(Integer,Integer)]
readIntegerPair = map (pair . map readInteger . B.words) . B.lines <$> B.getContents

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

readInteger :: B.ByteString -> Integer
readInteger = fst . fromJust . B.readInteger

pair :: [a] -> (a, a)
pair [x, y] = (x, y)
pair _ = undefined
