module Graph (
  Vertex,
  Vertexes,
  Edge,
  Graph,

  buildG,
  size,
  from,
  degree,
  readUndirectedEdge,
  readDirectedEdge
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import qualified Data.Array.IArray as A
import Data.Maybe

type Vertex = Int
type Vertexes = S.IntSet
type Edge = (Vertex, Vertex)
type Graph = A.Array Vertex Vertexes

buildG :: Int -> [Edge] -> Graph
buildG n = A.accumArray (flip S.insert) S.empty (1, n)

size :: Graph -> Int
size = snd . A.bounds

from :: Graph -> Vertex -> Vertexes
from g = (g A.!)

degree :: Graph -> Vertex -> Int
degree g = S.size . (g A.!)

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map (constructOneWay . map readInt . B.words) . B.lines

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap (constructTwoWay . map readInt . B.words) . B.lines

constructOneWay :: [Int] -> Edge
constructOneWay [s,t] = (s, t)
constructOneWay _ = undefined

constructTwoWay :: [Int] -> [Edge]
constructTwoWay [s,t] = [(s, t), (t, s)]
constructTwoWay _ = undefined

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt
