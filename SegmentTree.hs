{-# OPTIONS_GHC -fno-warn-orphans #-}

module SegmentTree (
  SegTree,
  val,
  fromList,
  update,
  query
) where

import Data.Monoid

-- modify according to your purpose
instance Monoid Int where
  mempty = 0
  mappend v w = v + w

data SegTree m =
  Leaf !m |
  Node !m !(SegTree m) !(SegTree m)
  deriving (Eq, Show)

type Index = Int
type Size = Int

val :: Monoid m => SegTree m -> m
val (Leaf v) = v
val (Node v _ _) = v

fromList :: Monoid m => Size -> [m] -> SegTree m
fromList 1 [x] = Leaf x
fromList n xs = Node (val left <> val right) left right
  where
    m = div n 2
    (xs1, xs2) = splitAt m xs
    left = fromList m xs1
    right = fromList (n - m) xs2

update :: Monoid m => Size -> SegTree m -> Index -> m -> SegTree m
update 1 (Leaf v) 1 x = Leaf (v <> x)
update _ (Leaf _) _ _ = undefined
update n (Node _ l r) i x
  | i <= m = Node (val left <> val r) left r
  | otherwise = Node (val l <> val right) l right
  where
    m = div n 2
    left = update m l i x
    right = update (n - m) r (i - m) x

query :: Monoid m => Size -> SegTree m -> Index -> Index -> m
query 1 (Leaf v) 1 1 = v
query _ (Leaf _) _ _ = undefined
query n (Node v l r) i j
  | (i, j) == (1, n) = v
  | j <= m = query m l i j
  | i > m = query (n - m) r (i - m) (j - m)
  | otherwise = query m l i m <> query (n - m) r 1 (j - m)
  where m = div n 2
