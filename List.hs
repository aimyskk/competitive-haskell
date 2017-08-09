module List (
  splitOn,
  comb,
  divides,
  lis
) where

import Control.Monad
import qualified Data.Set as S
import Data.List

splitOn :: Int -> [a] -> [[a]]
splitOn n = unfoldr (\xs -> guard ((not . null) xs) >> return (splitAt n xs))

comb :: Int -> [a] -> [[a]]
comb n xs = go n xs [] []
  where
    go 0 _ ys zs = reverse ys : zs
    go _ [] _  zs = zs
    go m (w:ws) ys zs = go (m - 1) ws (w : ys) (go m ws ys zs)

divides :: [a] -> [[[a]]]
divides [] = []
divides [x] = [[[x]]]
divides (x:xs) = let dxs = divides xs in map ([x] :) dxs ++ map (\(ys:yss) -> (x:ys):yss) dxs

lis :: Ord a => [a] -> [a]
lis = S.toList . snd . foldl _lis (S.empty, S.empty)

_lis :: Ord a => (S.Set a, S.Set a) -> a -> (S.Set a, S.Set a)
_lis (acc, acc0) x = case S.lookupGT x acc of
  Nothing -> let acc1 = S.insert x acc in (acc1, acc1)
  Just g -> (S.insert x (S.delete g acc), acc0)
