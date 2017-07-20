module Graph.Algorithm.BellmanFord (
  bellmanFord,
) where

import Graph.WeightedGraph

import qualified Data.Set as S
import qualified Data.IntMap as M

type Path = [Vertex]
type Memo = M.IntMap (Weight, Path)

-- accumrated weight and lexical order shortest path
bellmanFord :: Graph -> Vertex -> Memo
bellmanFord g s = bellmanFordCore g n m0
 where
  n = size g
  m0 = M.singleton s (0, [])

bellmanFordCore :: Graph -> Int -> Memo -> Memo
bellmanFordCore g n m
  | n == 1 = m
  | otherwise = bellmanFordCore g (n - 1) (M.foldrWithKey (move g) m m)

move :: Graph -> Vertex -> (Weight, Path) -> Memo -> Memo
move g s (aw, path) m = S.foldr update m (from g s)
  where
    update (t, w) acc
      | M.notMember t acc || fst (acc M.! t) < aw + w = M.insert t (aw + w, t : path) acc
      | fst (acc M.! t) == aw + w = M.insertWith lexmin t (aw + w, t : path) acc
      | otherwise = acc

lexmin :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
lexmin (w1, p1) (_, p2) = (w1, min p1 p2)
