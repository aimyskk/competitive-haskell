module Array.Syakutori (
  buildA,
  nsum
) where

import Data.Array.Unboxed

type Index = Int

-- 1-indexed
buildA :: IArray a e => [e] -> a Index e
buildA xs = listArray (1, length xs) xs

nsum :: (Num e, Ord e, IArray a e) => e -> a Index e -> Int
nsum x arr = go 1 1 (arr ! 1)
  where
    (_, l) = bounds arr
    go i j acc
      | i == l && j == l = if acc == x then 1 else 0
      | i == j = (if acc == x then 1 else 0) + go i (j + 1) (acc + arr ! (j + 1))
      | j == l = (if acc == x then 1 else 0) + go (i + 1) j (acc - arr ! i)
      | acc == x = 1 + go i (j + 1) (acc + arr ! (j + 1))
      | acc < x = go i (j + 1) (acc + arr ! (j + 1))
      | acc > x = go (i + 1) j (acc - arr ! i)
      | otherwise = error "syakutori: Pattern match failed."
