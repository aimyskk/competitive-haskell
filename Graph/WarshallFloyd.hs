module Graph.WarshallFloyd (
  warshallFloyd
) where

import qualified Data.Map.Strict as M

import WeightedGraph

type Memo = M.Map (Vertex, Vertex) Weight

-- 0-indexed vertex
warshallFloyd :: Int -> [Edge] -> Memo
warshallFloyd n es = foldl shorten m0 kij
  where
    m0 = M.fromList es
    kij = [(k, i, j) | k <- [0 .. pred n], i <- [0 .. pred n], j <- [0 .. pred n]]

shorten :: Memo -> (Vertex, Vertex, Vertex) -> Memo
shorten m (k, i, j) = case connect m k i j of
  Nothing -> m
  Just w -> M.insertWith min (i, j) w m

connect :: Memo -> Vertex -> Vertex -> Vertex -> Maybe Weight
connect m k i j = do
  w1 <- M.lookup (i, k) m
  w2 <- M.lookup (k, j) m
  return (w1 + w2)
