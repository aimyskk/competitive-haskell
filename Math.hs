module Math (
  primes,
  factorization,
  isqrt
) where

import Data.List

primes :: [Int]
primes = 2 : 3 : [x | i <- [1..], j <- [-1,1], let x = 6*i+j, isPrime x]
  where
    isPrime n = null [i | i <- takeWhile (\p -> p*p <= n) primes, mod n i == 0]

factorization :: Int -> [Int]
factorization n = unfoldr go (n, primes)
  where
    go (_, []) = Nothing
    go (m, pps@(p:ps))
      | m == 1 = Nothing
      | p > isqrt n = Just (m, (1, pps))
      | otherwise = let (q,r) = divMod m p in if r == 0 then Just (p, (q,pps)) else go (m,ps)

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . (fromIntegral :: Integral a => a -> Double)
