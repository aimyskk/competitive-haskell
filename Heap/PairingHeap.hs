-- Chris Okasaki: Purely Functional Data Structure

module Heap.PairingHeap (
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

data PairingHeap a = E | T a [PairingHeap a]

mergePairs :: Ord a => [PairingHeap a] -> PairingHeap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)

instance Heap PairingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = insert x empty
  fromList = foldr insert empty

  insert x = merge (T x [])

  merge h E = h
  merge E h = h
  merge h1@(T x hs1) h2@(T y hs2)
    | x < y = T x (h2 : hs1)
    | otherwise = T y (h1 : hs2)

  findMin E = error "empty heap"
  findMin (T x _) = x

  deleteMin E = error "empty heap"
  deleteMin (T _ hs) = mergePairs hs

  deleteFindMin E = error "empty heap"
  deleteFindMin (T x hs) = (x, mergePairs hs)
