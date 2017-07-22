-- Chris Okasaki: Purely Functional Data Structure

module Heap.SplayHeap (
  SplayHeap,
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

data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a)

partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition _ E = (E, E)
partition pivot t@(T a x b)
  | x <= pivot = case b of
    E -> (t, E)
    T b1 y b2
      | y <= pivot -> let (small,big) = partition pivot b2 in (T (T a x b) y small, big)
      | otherwise -> let (small,big) = partition pivot b1 in (T a x small, T big y b2)
  | otherwise = case a of
    E -> (E, t)
    T a1 y a2
      | y <= pivot -> let (small,big) = partition pivot a2 in (T a1 y small, T big x b)
      | otherwise -> let (small,big) = partition pivot a1 in (small, T big y (T a2 x b))

instance Heap SplayHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = insert x empty
  fromList = foldr insert empty

  insert x t = T a x b
    where
      (a, b) = partition x t

  merge E t = t
  merge (T a x b) t = T (merge ta a) x (merge tb b)
    where
      (ta, tb) = partition x t

  findMin E = error "empty heap"
  findMin (T E x _) = x
  findMin (T a _ _) = findMin a

  deleteMin E = error "empty heap"
  deleteMin (T E _ b) = b
  deleteMin (T (T E _ b) y c) = T b y c
  deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)

  deleteFindMin E = error "empty heap"
  deleteFindMin (T E x b) = (x, b)
  deleteFindMin (T t@(T E _ b) y c) = (findMin t, T b y c)
  deleteFindMin (T t@(T a x b) y c) = (findMin t, T (deleteMin a) x (T b y c))
