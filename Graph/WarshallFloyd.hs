module Graph.WarshallFloyd (
  warshallFloyd,
  readDirectedEdge,
  readUndirectedEdge
) where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.Maybe

type Vertex = Int
type Weight = Int
type Edge = ((Vertex, Vertex), (Weight, Path))
type Path = [Vertex]
type Memo = M.Map (Vertex, Vertex) (Weight, Path)

-- accumrated weight and lexical order shortest path
warshallFloyd :: Int -> [Edge] -> Memo
warshallFloyd n es = foldl shorten m0 kij
  where
    m0 = M.fromList es
    kij = [(k,i,j) | k<-[0..n-1], i<-[0..n-1], j<-[0..n-1]]

shorten :: Memo -> (Vertex, Vertex, Vertex) -> Memo
shorten m (k, i, j) = case connect m k i j of
  Nothing -> m
  Just (w, p) -> M.insertWith lexmin (i, j) (w, p) m

connect :: Memo -> Vertex -> Vertex -> Vertex -> Maybe (Weight, Path)
connect m k i j = do
  (w1, p1) <- M.lookup (i, k) m
  (w2, p2) <- M.lookup (k, j) m
  return (w1 + w2, init p1 ++ tail p2)

lexmin :: (Weight, Path) -> (Weight, Path) -> (Weight, Path)
lexmin (w1, p1) (w2, p2)
  | w1 < w2 = (w1, p1)
  | w1 == w2 = (w1, min p1 p2)
  | otherwise = (w2, p2)

readDirectedEdge :: B.ByteString -> [Edge]
readDirectedEdge = map (constructOneWay . map readInt . B.words) . B.lines

readUndirectedEdge :: B.ByteString -> [Edge]
readUndirectedEdge = concatMap (constructTwoWay . map readInt . B.words) . B.lines

constructOneWay :: [Int] -> Edge
constructOneWay [s,t,w] = ((s,t), (w,[s,t]))
constructOneWay _ = undefined

constructTwoWay :: [Int] -> [Edge]
constructTwoWay [s,t,w] = [((s,t), (w,[s,t])), ((t,s), (w,[t,s]))]
constructTwoWay _ = undefined

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt
