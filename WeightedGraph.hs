module WeightedGraph (
  buildG,
  from,
  degree,
  dijkstra,
  readUndirectedEdge,
  readDirectedEdge
) where

import Data.IntMap (IntMap, (!))
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

-- provisional implementation
type Heap = S.Set (Weight, Vertex)

nullq :: Heap -> Bool
nullq = S.null

-- empty :: Heap
-- empty = S.empty

push :: Weight -> Vertex -> Heap -> Heap
push c v = S.insert (c, v)

pop :: Heap -> ((Weight, Vertex), Heap)
pop = S.deleteFindMin

buildG :: Int -> [Edge] -> Graph
buildG n = A.accumArray (flip S.insert) S.empty (1, n)

from :: Graph -> Vertex -> S.Set (Vertex, Weight)
from g = (g A.!)

degree :: Graph -> Vertex -> Int
degree g = S.size . (g A.!)

-- accumrated weight and lexical order shortest path
dijkstra :: Graph -> Heap -> IntMap (Weight, Path) -> IntMap (Weight, Path)
dijkstra g q im
  | nullq q = im
  | otherwise = dijkstra g q2 im1
  where
    ((c0, v), q1) = pop q
    r = v : snd (im ! v)
    im1 = S.foldr (\(b,c) acc -> M.insertWith mindic b (c0+c,r) acc) im vns
    q2 = S.foldr (\(b,c) acc -> push (c0+c) b acc) q1 vns
    vns = S.filter (\(b,c) -> M.notMember b im || c0+c <= fst (im!b)) (from g v)

mindic :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
mindic (_, []) (_, _) = undefined
mindic (c1, r) (c2, v2)
  | c1 < c2 = (c1, r)
  | otherwise = (c1, min r v2)

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap ((\[v,w,l] -> [(v,(w,l)),(w,(v,l))]) . map readVertex . B.words) . B.lines

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map ((\[v,w,l] -> (v,(w,l))) . map readVertex . B.words) . B.lines

readVertex :: B.ByteString -> Vertex
readVertex = fst . fromJust . B.readInt
