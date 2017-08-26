module Function where

import Data.List

untilFix :: Eq t => (t -> t) -> t -> t
untilFix f x = let x1 = f x in if x1 == x then x else untilFix f x1

comb :: Integral a => a -> a -> a
comb n r = div (fact n) (fact r * fact (n - r))
  where
    fact x = product [1 .. x]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . unfoldr (return . splitAt n)

combList :: Int -> [a] -> [[a]]
combList n xs = go n xs [] []
  where
    go 0 _ ys zs = reverse ys : zs
    go _ [] _  zs = zs
    go m (w:ws) ys zs = go (m - 1) ws (w : ys) (go m ws ys zs)

divides :: [a] -> [[[a]]]
divides [] = []
divides [x] = [[[x]]]
divides (x:xs) = let dxs = divides xs in map ([x] :) dxs ++ map (\(ys:yss) -> (x:ys):yss) dxs
