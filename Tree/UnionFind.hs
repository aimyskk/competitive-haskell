module UnionFind (
  UnionFind,

  initial,
  merge,
  same
) where

import qualified Data.IntMap as M

import Function (untilFix)

data UnionFind = UnionFind {tree :: M.IntMap Point, rk :: M.IntMap Rank}
type Point = Int  -- p > 0
type Rank = Int

initial :: UnionFind
initial = UnionFind (M.singleton 0 0) M.empty

(!?) :: UnionFind -> Point -> Point
(!?) uf p = M.findWithDefault 0 p (tree uf)

rep :: UnionFind -> Point -> Point
rep uf = untilFix (uf !?)

rank :: UnionFind -> Point -> Rank
rank uf = (rk uf M.!)

isolate :: UnionFind -> Point -> Bool
isolate uf p = rep uf p == 0

merge :: UnionFind -> Point -> Point -> UnionFind
merge uf p1 p2
  | i1 && i2 = UnionFind (M.insert p2 p1 (M.insert p1 p1 (tree uf))) (M.insert p1 2 (rk uf))
  | i1 = UnionFind (M.insert p1 rp2 (tree uf)) (M.insert rp2 (rk2 + 1) (rk uf))
  | i2 = UnionFind (M.insert p2 rp1 (tree uf)) (M.insert rp1 (rk1 + 1) (rk uf))
  | rp1 /= rp2 && rk1 > rk2 = UnionFind (M.insert p2 rp1 (M.insert rp2 rp1 (tree uf))) (M.insert rp1 (rk1 + rk2) (rk uf))
  | rp1 /= rp2 = UnionFind (M.insert p1 rp2 (M.insert rp1 rp2 (tree uf))) (M.insert rp2 (rk1 + rk2) (rk uf))
  | otherwise = uf
  where
    i1 = isolate uf p1
    i2 = isolate uf p2
    rk1 = rank uf rp1
    rk2 = rank uf rp2
    rp1 = rep uf p1
    rp2 = rep uf p2

same :: UnionFind -> Point -> Point -> Bool
same uf p1 p2
  | p1 == p2 = True
  | isolate uf p1 = False
  | otherwise = rep uf p1 == rep uf p2
