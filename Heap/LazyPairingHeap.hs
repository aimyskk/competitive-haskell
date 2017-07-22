-- Chris Okasaki: Purely Functional Data Structure

module Heap.LazyPairingHeap (
  PairingHeap,
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

data PairingHeap a = E | T a (PairingHeap a) (PairingHeap a)

link :: Ord a => PairingHeap a -> PairingHeap a -> PairingHeap a
link E _ = undefined
link (T x E m) a = T x a m
link (T x b m) a = T x E (merge (merge a b) m)

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = insert x empty
  fromList = foldr insert empty

  insert x = merge (T x E E)

  merge a E = a
  merge E b = b
  merge a@(T x _ _) b@(T y _ _)
    | x <= y = link a b
    | otherwise = link b a

  findMin E = error "empty heap"
  findMin (T x _ _) = x

  deleteMin E = error "empty heap"
  deleteMin (T _ a m) = merge a m

  deleteFindMin E = error "empty heap"
  deleteFindMin (T x a m) = (x, merge a m)
