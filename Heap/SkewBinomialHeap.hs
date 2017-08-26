-- Chris Okasaki: Purely Functional Data Structure

module Heap.SkewBinomialHeap (
  SkewBinomialHeap,
  empty,
  isEmpty,
  singleton,
  fromList,
  insert,
  merge,
  findMin,
  deleteMin,
  deleteFindMin
) where

import Class

data Tree a = Node Int a [a] [Tree a]

newtype SkewBinomialHeap a = SBH [Tree a]

rank :: Tree a -> Int
rank (Node r _ _ _) = r

root :: Tree a -> a
root (Node _ x _ _) = x

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 xs1 c1) t2@(Node _ x2 xs2 c2)
  | x1 <= x2 = Node (r + 1) x1 xs1 (t2 : c1)
  | otherwise = Node (r + 1) x2 xs2 (t1 : c2)

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2
  | x <= y = Node r x (y : ys) c
  | otherwise = Node r y (x : ys) c
  where
    Node r y ys c = link t1 t2

insTree :: Ord a => Tree a -> [Tree a] -> [Tree a]
insTree t [] = [t]
insTree t ts@(t' : ts')
  | rank t < rank t' = t : ts
  | otherwise = insTree (link t t') ts'

mrg :: Ord a => [Tree a] -> [Tree a] -> [Tree a]
mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1 : ts1') ts2@(t2 : ts2')
  | rank t1 < rank t2 = t1 : mrg ts1' ts2
  | rank t2 < rank t1 = t2 : mrg ts1 ts2'
  | otherwise = insTree (link t1 t2) (mrg ts1' ts2')

normalize :: Ord a => [Tree a] -> [Tree a]
normalize [] = []
normalize (t : ts) = insTree t ts

removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
removeMinTree [] = error "empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t : ts)
  | root t < root t' = (t, ts)
  | otherwise = (t', t : ts')
  where
    (t', ts') = removeMinTree ts

instance Heap SkewBinomialHeap where
  empty = SBH []
  isEmpty (SBH ts) = null ts

  singleton x = insert x empty
  fromList = foldr insert empty

  insert x (SBH (t1 : t2 : ts))
    | rank t1 == rank t2 = SBH (skewLink x t1 t2 : ts)
  insert x (SBH ts) = SBH (Node 0 x [] [] : ts)

  merge (SBH ts1) (SBH ts2) = SBH (mrg (normalize ts1) (normalize ts2))

  findMin (SBH ts) = root t
    where
      (t, _) = removeMinTree ts

  deleteMin (SBH ts) = foldr insert (SBH ts') xs
    where
      (Node _ _ xs ts1, ts2) = removeMinTree ts
      ts' = mrg (reverse ts1) (normalize ts2)

  deleteFindMin (SBH ts) = (root t, foldr insert (SBH ts') xs)
    where
      (t, _) = removeMinTree ts
      (Node _ _ xs ts1, ts2) = removeMinTree ts
      ts' = mrg (reverse ts1) (normalize ts2)
