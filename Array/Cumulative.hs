module Array.Cumulative where

import Data.List
import qualified Data.Array.IArray as A

type Height = Int
type Width = Int
type Coord = (Height, Width)

-- 1-indexed
type Matrix a = A.Array Height (A.Array Width a)

fromList :: Num a => Height -> Width -> [[a]] -> Matrix a
fromList h w = A.listArray (1, h) . map (A.listArray (1, w))

fromListAccum :: Num a => Height -> Width -> [[a]] -> Matrix a
fromListAccum h w = A.listArray (1, h) . map (A.listArray (1, w)) . csum2

csum1 :: Num a => [a] -> [a]
csum1 = scanl1 (+)

csum2 :: Num a => [[a]] -> [[a]]
csum2 = transpose . map csum1 . transpose . map csum1

(!) :: Matrix a -> Coord -> a
(!) m (x, y) = (m A.! x) A.! y

rsum :: Num a => Matrix a -> Coord -> Coord -> a
rsum m (x1,y1) c2@(x2,y2) = a1 - a2 - a3 + a4
  where
    a1 = m ! c2
    a2 = if x1 == 1 then 0 else m ! (x1-1, y2)
    a3 = if y1 == 1 then 0 else m ! (x2, y1-1)
    a4 = if x1 == 1 || y1 == 1 then 0 else m ! (x1-1, y1-1)
