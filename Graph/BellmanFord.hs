module Graph.BellmanFord (
  bellmanFord
) where

import WeightedGraph

import qualified Data.Set as S
import qualified Data.IntMap.Strict as M

type Memo = M.IntMap Weight

bellmanFord :: Graph -> Vertex -> Memo
bellmanFord g s = _bellmanFord g n m0
 where
  n = size g
  m0 = M.singleton s 0

_bellmanFord :: Graph -> Int -> Memo -> Memo
_bellmanFord g n m
  | n == 0 = m
  | otherwise = _bellmanFord g (pred n) (M.foldrWithKey (move g) m m)

move :: Graph -> Vertex -> Weight -> Memo -> Memo
move g s aw m = S.foldr update m (from g s)
  where
    update (t, w) acc
      | M.notMember t acc || acc M.! t < aw + w = M.insert t (aw + w) acc
      | acc M.! t == aw + w = M.insertWith min t (aw + w) acc
      | otherwise = acc
