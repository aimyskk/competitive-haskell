module Array.Cumulative where

import Data.List
import qualified Data.Array.Base as A

type Height = Int
type Width = Int
type Coord = (Height, Width)
type Index = Int

-- 1-indexed
data IMatrix = M {
  _height :: Int,
  _width :: Int,
  _matrix :: A.UArray Index Int
} deriving (Eq, Show)

(!) :: IMatrix -> Coord -> Int
(!) m c = A.unsafeAt (_matrix m) (idx (_width m) c - 1)

idx :: Width -> Coord -> Index
idx w (x, y) = (x - 1) * w + y

fromList :: Height -> Width -> [[Int]] -> IMatrix
fromList h w = M h w . A.listArray (1, h * w) . concat

fromListAccum :: Height -> Width -> [[Int]] -> IMatrix
fromListAccum h w = fromList h w . csum2

csum1 :: Num a => [a] -> [a]
csum1 = scanl1 (+)

csum2 :: Num a => [[a]] -> [[a]]
csum2 = transpose . map csum1 . transpose . map csum1

rsum :: IMatrix -> Coord -> Coord -> Int
rsum m (x1,y1) c2@(x2,y2) = a1 - a2 - a3 + a4
  where
    a1 = m ! c2
    a2 = if x1 == 1 then 0 else m ! (x1 - 1, y2)
    a3 = if y1 == 1 then 0 else m ! (x2, y1 - 1)
    a4 = if x1 == 1 || y1 == 1 then 0 else m ! (x1 - 1, y1 - 1)
