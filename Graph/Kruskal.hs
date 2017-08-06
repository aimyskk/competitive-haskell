module Graph.Kruskal (
  kruskal
) where

import Data.List

import WeightedGraph
import Tree.UnionFind

kruskal :: [Edge] -> Int
kruskal = fst . _kruskal initial 0 . sortOn snd

_kruskal :: UnionFind -> Int -> [Edge] -> (Int, UnionFind)
_kruskal uf acc [] = (acc, uf)
_kruskal uf acc (((a,b),d):abd)
  | same uf a b = _kruskal uf acc abd
  | otherwise = _kruskal (merge uf a b) (acc + d) abd
