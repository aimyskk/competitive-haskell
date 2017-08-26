module Graph.Dijkstra (
  dijkstra
) where

import WeightedGraph

import qualified Data.Set as S
import qualified Data.IntMap.Strict as M

type Memo = M.IntMap Weight

dijkstra :: Graph -> Vertex -> Memo
dijkstra g s = _dijkstra g q0 m0
  where
   q0 = singleton (0, s)
   m0 = M.singleton s 0

_dijkstra :: Graph -> PriorityQueue -> Memo -> Memo
_dijkstra g q m
  | isEmpty q = m
  | otherwise = _dijkstra g q2 m1
  where
    ((w0, s), q1) = deleteFindMin q
    m1 = S.foldr (\(t, w) acc -> M.insertWith min t (w0 + w) acc) m vns
    q2 = S.foldr (\(t, w) acc -> insert (w0 + w, t) acc) q1 vns
    vns = S.filter (\(t, w) -> M.notMember t m || w0 + w <= m M.! t) (target g s)

type PriorityQueue = S.Set (Weight, Vertex)

isEmpty :: PriorityQueue -> Bool
isEmpty = S.null

singleton :: (Weight, Vertex) -> PriorityQueue
singleton = S.singleton

insert :: (Weight, Vertex) -> PriorityQueue -> PriorityQueue
insert = S.insert

deleteFindMin :: PriorityQueue -> ((Weight, Vertex), PriorityQueue)
deleteFindMin = S.deleteFindMin
