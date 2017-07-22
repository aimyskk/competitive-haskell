module Graph.DFS (
  dfs
) where

import Graph

import qualified Data.IntSet as S

-- modify according to your purpose
type Acc = Int

dfs :: Graph -> (Vertex -> Acc -> Acc) -> Vertex -> Acc
dfs g p v = fst $ dfsCore g p v (1, visited)
  where
   visited = S.singleton v

dfsCore :: Graph -> (Vertex -> Acc -> Acc) -> Vertex -> (Acc, Vertexes) -> (Acc, Vertexes)
dfsCore g p v (m, visited)
  | S.null next = (m, visited)
  | otherwise = S.foldr (branch g p) (m, visited) next
  where
    next = S.difference (from g v) visited

branch :: Graph -> (Vertex -> Acc -> Acc) -> Vertex -> (Acc, Vertexes) -> (Acc, Vertexes)
branch g p w (aa,avd) = dfsCore g p w (p w aa, S.insert w avd)
