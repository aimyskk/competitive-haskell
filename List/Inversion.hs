module List.Inversion (
  inversion
) where

import Tree.SegmentTree

inversion :: Int -> [Int] -> Int
inversion n = fst . foldl go (0, seg0) . zip [0 .. pred n]
  where
    seg0 = fromList n (replicate n mempty)
    go (acc, seg) (j, a) = (acc + (j - query n seg 1 a), update n seg a succ)
