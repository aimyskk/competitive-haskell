module WeightedGraph (
  buildG,
  size,
  from,
  degree,
  bellmanFord,
  dijkstra,
  readUndirectedEdge,
  readDirectedEdge
) where

import Data.IntMap ((!))
import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Array.IArray as A
import Data.Maybe

type Vertex = Int
type Weight = Int
type Edge = (Vertex, (Vertex, Weight))
type Path = [Vertex]
type Graph = A.Array Vertex (S.Set (Vertex, Weight))
type Memo = M.IntMap (Weight, Path)

-- provisional implementation
type Heap = S.Set (Weight, Vertex)

nullq :: Heap -> Bool
nullq = S.null

empty :: Heap
empty = S.empty

push :: Weight -> Vertex -> Heap -> Heap
push c v = S.insert (c, v)

pop :: Heap -> ((Weight, Vertex), Heap)
pop = S.deleteFindMin

buildG :: Int -> [Edge] -> Graph
buildG n = A.accumArray (flip S.insert) S.empty (1, n)

size :: Graph -> Int
size = snd . A.bounds

from :: Graph -> Vertex -> S.Set (Vertex, Weight)
from g = (g A.!)

degree :: Graph -> Vertex -> Int
degree g = S.size . (g A.!)

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
      | M.notMember t acc || fst (acc!t) < aw + w = M.insert t (aw + w, t : path) acc
      | fst (acc!t) == aw + w = M.insertWith lexmin t (aw + w, t : path) acc
      | otherwise = acc

lexmin :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
lexmin (w1, p1) (_, p2) = (w1, min p1 p2)

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
    p = s : snd (m ! s)
    m1 = S.foldr (\(t,w) acc -> M.insertWith mindic t (w0+w,p) acc) m vns
    q2 = S.foldr (\(t,w) acc -> push (w0+w) t acc) q1 vns
    vns = S.filter (\(t,w) -> M.notMember t m || w0+w <= fst (m!t)) (from g s)

mindic :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
mindic (w1, p1) (w2, p2)
  | w1 < w2 = (w1, p1)
  | otherwise = (w1, min p1 p2)

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap ((\[s,t,w] -> [(s,(t,w)),(t,(s,w))]) . map readVertex . B.words) . B.lines

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map ((\[s,t,w] -> (s,(t,w))) . map readVertex . B.words) . B.lines

readVertex :: B.ByteString -> Vertex
readVertex = fst . fromJust . B.readInt
