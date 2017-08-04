module UnionFind (
  UnionFind,
  rep,
  rank,
  merge,
  same,
  fromList,
  fromAscList,
  fromDistinctAscList
) where

import qualified Data.IntMap as M

import Function (untilFix)

data UnionFind = UnionFind {tree :: M.IntMap Point, rk :: M.IntMap Rank}
type Point = Int
type Rank = Int

rep :: UnionFind -> Point -> Point
rep uf = untilFix (tree uf M.!)

rank :: UnionFind -> Point -> Rank
rank uf = (M.!) (rk uf)

merge :: UnionFind -> Point -> Point -> UnionFind
merge uf p1 p2
  | rp1 /= rp2 && rk1 > rk2 = UnionFind tree12 rank1
  | rp1 /= rp2 = UnionFind tree21 rank2
  | otherwise = uf
  where
    tree12 = M.insert p2 rp1 (M.insert rp2 rp1 (tree uf))
    tree21 = M.insert p1 rp2 (M.insert rp1 rp2 (tree uf))
    rank1 = M.insert rp1 (rk1 + rk2) (rk uf)
    rank2 = M.insert rp2 (rk1 + rk2) (rk uf)
    rk1 = rank uf rp1
    rk2 = rank uf rp2
    rp1 = rep uf p1
    rp2 = rep uf p2

same :: UnionFind -> Point -> Point -> Bool
same uf p1 p2 = rep uf p1 == rep uf p2

fromList :: [Point] -> UnionFind
fromList ps = UnionFind tree0 rk0
  where
    tree0 = M.fromList [(i, i) | i <- ps]
    rk0 = M.fromList [(i, 1) | i <- ps]

fromAscList :: [Point] -> UnionFind
fromAscList ps = UnionFind tree0 rk0
  where
    tree0 = M.fromAscList [(i, i) | i <- ps]
    rk0 = M.fromAscList [(i, 1) | i <- ps]

fromDistinctAscList :: [Point] -> UnionFind
fromDistinctAscList ps = UnionFind tree0 rk0
  where
    tree0 = M.fromDistinctAscList [(i, i) | i <- ps]
    rk0 = M.fromDistinctAscList [(i, 1) | i <- ps]
