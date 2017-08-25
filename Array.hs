module Array (
  buildA,
  syakutori
) where

import Data.Array.Unboxed

type Index = Int

buildA :: [Int] -> UArray Index Int
buildA xs = listArray (1, length xs) xs

-- 1-indexed
syakutori :: Int -> UArray Index Int -> Int
syakutori n arr = go 1 1 (arr ! 1)
  where
    (_, l) = bounds arr
    go i j acc
      | i == l && j == l = if acc == n then 1 else 0
      | i == j = (if acc == n then 1 else 0) + go i (j + 1) (acc + arr ! (j + 1))
      | j == l = (if acc == n then 1 else 0) + go (i + 1) j (acc - arr ! i)
      | acc == n = 1 + go i (j + 1) (acc + arr ! (j + 1))
      | acc < n = go i (j + 1) (acc + arr ! (j + 1))
      | acc > n = go (i + 1) j (acc - arr ! i)
      | otherwise = error "syakutori: Pattern match failed."
