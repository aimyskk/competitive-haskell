module Tree.UnionFind where

import qualified Data.IntMap.Strict as M

data UnionFind = UnionFind {
  tree :: M.IntMap Point,
  rk :: M.IntMap Rank
} deriving (Eq, Show)

type Point = Int -- greater than 0
type Rank = Int

initial :: UnionFind
initial = UnionFind M.empty M.empty

rep :: UnionFind -> Point -> Point
rep uf p
  | M.notMember p (tree uf) = p
  | tree uf M.! p == p = p
  | otherwise = rep uf (tree uf M.! p)

rank :: UnionFind -> Point -> Rank
rank uf p = M.findWithDefault 1 p (rk uf)

merge :: UnionFind -> Point -> Point -> UnionFind
merge uf p q
  | rp1 == rp2 = uf
  | otherwise = UnionFind (M.insert rp2 rp1 (M.insert p2 rp1 (tree uf))) (M.insert rp1 (rk1 + rk2) (rk uf))
  where
    p1 = ordBy uf p q
    p2 = if p1 == p then q else p
    rp1 = rep uf p1
    rp2 = rep uf p2
    rk1 = rank uf rp1
    rk2 = rank uf rp2

same :: UnionFind -> Point -> Point -> Bool
same uf p1 p2 = rep uf p1 == rep uf p2

ordBy :: UnionFind -> Point -> Point -> Point
ordBy uf x y
  | rx > ry = x
  | rx < ry = y
  | rep uf x < rep uf y = x
  | otherwise = y
  where
    rx = rank uf x
    ry = rank uf y
