module Math.Prime (
  primes,
  factorization
) where

import Data.List

primes :: Integral a => [a]
primes = 2 : 3 : [x | i <- [1 ..], j <- [-1, 1], let x = 6 * i + j, isPrime x]
  where
    isPrime n = null [i | i <- takeWhile (\p -> p * p <= n) primes, mod n i == 0]

factorization :: Integral a => a -> [a]
factorization n = unfoldr go (n, primes)
  where
    go (_, []) = Nothing
    go (m, pps@(p:ps))
      | m == 1 = Nothing
      | p > (floor . (sqrt :: Double -> Double) . fromIntegral) n = Just (m, (1, pps))
      | otherwise = let (q, r) = divMod m p in if r == 0 then Just (p, (q, pps)) else go (m, ps)
