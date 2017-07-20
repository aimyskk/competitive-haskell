module Graph.Algorithm.BFS (
  bfs,
  reachable
) where

import Graph.Graph

import qualified Data.IntSet as S

bfs :: Graph -> Vertex -> Vertexes
bfs g v = go (S.singleton v) (S.singleton v)
  where
    go acc border
      | S.null border = acc
      | otherwise = go (S.union acc next) next
        where next = S.difference (transMap g border) acc

reachable :: Graph -> Vertex -> Vertex -> Bool
reachable g v w = S.member w (bfs g v)

transMap :: Graph -> Vertexes -> Vertexes
transMap g = S.unions . map (from g) . S.elems
