module Graph (
  Vertex,
  Vertexes,
  Edge,
  Graph,

  buildG,
  from,
  size,
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
type Bound = (Vertex, Vertex)

type Vertex = Int
type Vertexes = S.IntSet
type Edge = (Vertex, Vertex)
type Graph = A.Array Vertex Vertexes

buildG :: Bound -> [Edge] -> Graph
buildG = A.accumArray (flip S.insert) S.empty

from :: Graph -> Vertex -> Vertexes
from = (A.!)

size :: Graph -> Int
size g = let (i,j) = A.bounds g in j - i + 1

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map (constructOneWay . map readInt . B.words) . B.lines

constructOneWay :: [Int] -> Edge
constructOneWay [s,t] = (s, t)
constructOneWay _ = undefined

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap (constructTwoWay . map readInt . B.words) . B.lines

constructTwoWay :: [Int] -> [Edge]
constructTwoWay [s,t] = [(s, t), (t, s)]
constructTwoWay _ = undefined

readBitmap :: Width -> B.ByteString -> [Edge]
readBitmap w bs = foldr (scout w bs) [] [0 .. B.length bs]

scout :: Width -> B.ByteString -> Index -> [Edge] -> [Edge]
scout w bs i acc = zip (repeat i) neighbor ++ acc
  where
    neighbor = filter (\x -> inner x && movable x) [i-1, i+1, i-w, i+w]
    inner x = x >= 0 && x < B.length bs
    movable x = B.index bs x `elem` ".sg"
