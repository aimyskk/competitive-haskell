-- Chris Okasaki: Purely Functional Data Structure

module Heap.BootstrapHeap (
  BootstrapHeap,
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

data BootstrapHeap h a = E | H a (h (BootstrapHeap h a))

instance Eq a => Eq (BootstrapHeap h a) where
  H x _ == H y _ = x == y
  _ == _ = undefined

instance Ord a => Ord (BootstrapHeap h a) where
  H x _ <= H y _ = x <= y
  _ <= _ = undefined

instance Heap h => Heap (BootstrapHeap h) where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = insert x empty
  fromList = foldr insert empty

  insert x = merge (H x empty)

  merge E h = h
  merge h E = h
  merge h1@(H x p1) h2@(H y p2)
    | x <= y = H x (insert h2 p1)
    | otherwise = H y (insert h1 p2)

  findMin E = error "empty heap"
  findMin (H x _) = x

  deleteMin E = error "empty heap"
  deleteMin (H _ p)
    | isEmpty p = E
    | otherwise = H y (merge p1 p2)
    where
      H y p1 = findMin p
      p2 = deleteMin p

  deleteFindMin E = error "empty heap"
  deleteFindMin (H x p)
    | isEmpty p = (x, E)
    | otherwise = (x, H y (merge p1 p2))
    where
      H y p1 = findMin p
      p2 = deleteMin p
