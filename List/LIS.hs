module List.LIS (
  lis
) where

import qualified Data.Set as S

lis :: Ord a => [a] -> [a]
lis = S.toList . snd . foldl go (S.empty, S.empty)
  where
    go (acc, acc0) x = case S.lookupGT x acc of
      Nothing -> let acc1 = S.insert x acc in (acc1, acc1)
      Just g -> (S.insert x (S.delete g acc), acc0)
