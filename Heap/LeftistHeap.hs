-- Chris Okasaki: Purely Functional Data Structure

module Heap.LeftistHeap (
  LeftistHeap,
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

import Heap

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a)

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b
  | rank a >= rank b = T (rank b + 1) x a b
  | otherwise = T (rank a + 1) x b a

instance Heap LeftistHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = insert x empty
  fromList = foldr insert empty

  insert x = merge (T 1 x E E)

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
    | x <= y = makeT x a1 (merge b1 h2)
    | otherwise = makeT y a2 (merge h1 b2)

  findMin E = error "empty heap"
  findMin (T _ x _ _) = x

  deleteMin E = error "empty heap"
  deleteMin (T _ _ a b) = merge a b

  deleteFindMin E = error "empty heap"
  deleteFindMin (T _ x a b) = (x, merge a b)
