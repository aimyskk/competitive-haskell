module Graph.DFS (
  dfs
) where

import Graph

import qualified Data.IntSet as S

-- modify according to your purpose
type Acc = Bool
 
dfs :: Graph -> (Vertex -> Acc -> Acc) -> Acc -> Vertex -> Acc
dfs g f acc v = fst $ _dfs g f v (acc, visited)
  where
   visited = S.singleton v
 
_dfs :: Graph -> (Vertex -> Acc -> Acc) -> Vertex -> (Acc, Vertexes) -> (Acc, Vertexes)
_dfs g f v (acc, visited)
  | S.null next = (acc, visited)
  | otherwise = S.foldr (branch g f) (acc, visited) next
  where
    next = S.difference (from g v) visited
 
branch :: Graph -> (Vertex -> Acc -> Acc) -> Vertex -> (Acc, Vertexes) -> (Acc, Vertexes)
branch g f w (aa,avd) = _dfs g f w (f w aa, S.insert w avd)
