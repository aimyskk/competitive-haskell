module WeightedGraph (
  Vertex,
  Weight,
  Edge,
  Graph,

  buildG,
  size,
  from,
  readUndirectedEdge,
  readDirectedEdge
) where

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Array.IArray as A

import Scanner (readInt)

type Vertex = Int
type Weight = Int
type Edge = (Vertex, (Vertex, Weight))
type Graph = A.Array Vertex (S.Set (Vertex, Weight))

buildG :: Int -> [Edge] -> Graph
buildG n = A.accumArray (flip S.insert) S.empty (1, n)

size :: Graph -> Int
size = snd . A.bounds

from :: Graph -> Vertex -> S.Set (Vertex, Weight)
from g = (g A.!)

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map (constructOneWay . map readInt . B.words) . B.lines

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap (constructTwoWay . map readInt . B.words) . B.lines

constructOneWay :: [Int] -> Edge
constructOneWay [s,t,w] = (s, (t, w))
constructOneWay _ = undefined

constructTwoWay :: [Int] -> [Edge]
constructTwoWay [s,t,w] = [(s, (t,w)), (t, (s,w))]
constructTwoWay _ = undefined
