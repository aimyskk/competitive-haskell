module Graph.Graph (
  buildG,
  from,
  degree,
  reachable,
  path,
  readUndirectedEdge,
  readDirectedEdge
) where

import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as S
import Data.Array.IArray
import Data.Maybe

type Vertex = Int
type Vertexes = S.IntSet
type Edge = (Vertex, Vertex)
type Graph = Array Vertex Vertexes

buildG :: Int -> [Edge] -> Graph
buildG n = accumArray (flip S.insert) S.empty (1, n)

from :: Graph -> Vertex -> Vertexes
from g = (g !)

degree :: Graph -> Vertex -> Int
degree g = S.size . (g !)

reachable :: Graph -> Vertex -> Vertexes
reachable g v = go (S.singleton v) (S.singleton v)
  where
    go acc border
      | S.null border = acc
      | otherwise = go (S.union acc next) next
        where next = S.difference (transMap g border) acc

path :: Graph -> Vertex -> Vertex -> Bool
path g v w = S.member w (reachable g v)

transMap :: Graph -> Vertexes -> Vertexes
transMap g = S.unions . map (from g) . S.elems

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
