module Class where

-- Chris Okasaki: Purely Functional Data Structure
class Heap h where
  empty :: Ord a => h a
  isEmpty :: Ord a => h a -> Bool

  singleton :: Ord a => a -> h a
  fromList :: Ord a => [a] -> h a

  insert :: Ord a => a -> h a -> h a
  merge :: Ord a => h a -> h a -> h a

  findMin :: Ord a => h a -> a
  deleteMin :: Ord a => h a -> h a
  deleteFindMin :: Ord a => h a -> (a, h a)
