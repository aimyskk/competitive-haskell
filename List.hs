module List (
  splitOn,
  comb,
  divides
) where

import Control.Monad
import Data.List

splitOn :: Int -> [a] -> [[a]]
splitOn n = unfoldr (\xs -> guard ((not.null) xs) >> return (splitAt n xs))

comb :: Int -> [a] -> [[a]]
comb n xs = go n xs [] []
  where
    go 0 _ ys zs = reverse ys : zs
    go _ [] _  zs = zs
    go m (w:ws) ys zs = go (m - 1) ws (w : ys) (go m ws ys zs)

divides :: [a] -> [[[a]]]
divides [] = []
divides [x] = [[[x]]]
divides (x:xs) = let dxs = divides xs in map ([x]:) dxs ++ map (\(ys:yss) -> (x:ys):yss) dxs
