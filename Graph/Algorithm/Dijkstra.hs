module Graph.Algorithm.Dijkstra (
  dijkstra
) where

import Graph.WeightedGraph

import qualified Data.Set as S
import qualified Data.IntMap as M

type Path = [Vertex]
type Memo = M.IntMap (Weight, Path)

-- priority queue: provisional implementation
type Heap = S.Set (Weight, Vertex)

nullq :: Heap -> Bool
nullq = S.null

empty :: Heap
empty = S.empty

push :: Weight -> Vertex -> Heap -> Heap
push c v = S.insert (c, v)

pop :: Heap -> ((Weight, Vertex), Heap)
pop = S.deleteFindMin

-- accumrated weight and lexical order shortest path
dijkstra :: Graph -> Vertex -> Memo
dijkstra g s = dijkstraCore g q0 m0
  where
   q0 = push 0 s empty
   m0 = M.singleton s (0, [])

dijkstraCore :: Graph -> Heap -> Memo -> Memo
dijkstraCore g q m
  | nullq q = m
  | otherwise = dijkstraCore g q2 m1
  where
    ((w0, s), q1) = pop q
    p = s : snd (m M.! s)
    m1 = S.foldr (\(t,w) acc -> M.insertWith mindic t (w0+w,p) acc) m vns
    q2 = S.foldr (\(t,w) acc -> push (w0+w) t acc) q1 vns
    vns = S.filter (\(t,w) -> M.notMember t m || w0+w <= fst (m M.! t)) (from g s)

mindic :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
mindic (w1, p1) (w2, p2)
  | w1 < w2 = (w1, p1)
  | otherwise = (w1, min p1 p2)
