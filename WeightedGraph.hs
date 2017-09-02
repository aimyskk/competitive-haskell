module WeightedGraph where

import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Array.IArray as A

import Scanner

type Bound = (Vertex, Vertex)
type Vertex = Int
type Weight = Int
type Edge = ((Vertex, Vertex), Weight)
type Path = [Vertex]
type Graph = A.Array Vertex (S.Set (Vertex, Weight))

buildG :: Bound -> [Edge] -> Graph
buildG b = A.accumArray (flip S.insert) S.empty b . map (\((s, t), w) -> (s, (t, w)))

target :: Graph -> Vertex -> S.Set (Vertex, Weight)
target = (A.!)

size :: Graph -> Int
size g = let (i,j) = A.bounds g in j - i + 1

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map (constructOneWay . map readInt . B.words) . B.lines

constructOneWay :: [Int] -> Edge
constructOneWay [s,t,w] = ((s, t), w)
constructOneWay _ = undefined

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap (constructTwoWay . map readInt . B.words) . B.lines

constructTwoWay :: [Int] -> [Edge]
constructTwoWay [s,t,w] = [((s, t), w), ((t, s), w)]
constructTwoWay _ = undefined
