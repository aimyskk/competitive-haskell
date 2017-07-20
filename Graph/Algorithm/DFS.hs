module Graph.Algorithm.DFS (
  dfs
) where

import Graph.Graph

import qualified Data.IntSet as S
import Data.Monoid

dfs :: Monoid m => Graph -> Vertex -> m
dfs g v = dfsCore g v visited mempty
  where
    visited = S.empty

dfsCore :: Monoid m => Graph -> Vertex -> Vertexes -> m -> m
dfsCore g v visited m
  | S.null next = m
  | otherwise = S.foldr (\w acc -> acc <> dfsCore g w (S.insert v visited) m) mempty next
  where next = S.difference (from g v) visited
