module Graph (
  Vertex,
  Vertexes,
  Edge,
  Graph,

  buildG,
  size,
  from,
  readUndirectedEdge,
  readDirectedEdge,
  readBitmap
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import qualified Data.Array.IArray as A

import Scanner (readInt)

--type Height = Int
type Width = Int
type Index = Int

type Vertex = Int
type Vertexes = S.IntSet
type Edge = (Vertex, Vertex)
type Graph = A.Array Vertex Vertexes

-- 1-indexed
buildG :: Int -> [Edge] -> Graph
buildG n = A.accumArray (flip S.insert) S.empty (1, n)

size :: Graph -> Int
size = snd . A.bounds

from :: Graph -> Vertex -> Vertexes
from g = (g A.!)

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

readBitmap :: Width -> B.ByteString -> [Edge]
readBitmap w bs = foldl (scout w bs) [] [0 .. B.length bs]

scout :: Width -> B.ByteString -> [Edge] -> Index -> [Edge]
scout w bs acc i = zip (repeat i) neighbor ++ acc
  where
    neighbor = filter (\x -> inner x && movable x) [i-1, i+1, i-w, i+w]
    inner x = x >= 0 && x < B.length bs
    movable x = B.index bs x `elem` ".sg"
