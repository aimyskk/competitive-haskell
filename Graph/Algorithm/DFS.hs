module Graph.Algorithm.DFS (
  dfs
) where

import Graph.Graph

import qualified Data.IntSet as S
import Data.Monoid

dfs :: Monoid m => Graph -> (Vertexes -> m) -> Vertex -> m
dfs g p v = dfsCore g p v visited mempty
  where
    visited = S.empty

dfsCore :: Monoid m => Graph -> (Vertexes -> m) -> Vertex -> Vertexes -> m -> m
dfsCore g p v visited m
  | S.null next = p visited <> m
  | otherwise = S.foldr (\w acc -> acc <> dfsCore g p w (S.insert v visited) m) mempty next
  where
    next = S.difference (from g v) visited
