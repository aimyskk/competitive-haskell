module Graph.Dijkstra (
  dijkstra
) where

import WeightedGraph
import Heap.LeftistHeap

import qualified Data.Set as S
import qualified Data.IntMap as M

type Path = [Vertex]
type Memo = M.IntMap (Weight, Path)
type PriorityQueue = LeftistHeap (Weight, Vertex)

-- accumrated weight and lexical order shortest path
dijkstra :: Graph -> Vertex -> Memo
dijkstra g s = dijkstraCore g q0 m0
  where
   q0 = singleton (0, s)
   m0 = M.singleton s (0, [])

dijkstraCore :: Graph -> PriorityQueue -> Memo -> Memo
dijkstraCore g q m
  | isEmpty q = m
  | otherwise = dijkstraCore g q2 m1
  where
    ((w0, s), q1) = deleteFindMin q
    p = s : snd (m M.! s)
    m1 = S.foldr (\(t,w) acc -> M.insertWith mindic t (w0+w,p) acc) m vns
    q2 = S.foldr (\(t,w) acc -> insert (w0+w, t) acc) q1 vns
    vns = S.filter (\(t,w) -> M.notMember t m || w0+w <= fst (m M.! t)) (from g s)

mindic :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
mindic (w1, p1) (w2, p2)
  | w1 < w2 = (w1, p1)
  | otherwise = (w1, min p1 p2)
