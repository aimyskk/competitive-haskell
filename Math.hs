module Math (
  primes,
  factorization,
  isqrt,
  comb,
  modulus,
  (+%),
  (*%),
  powMod,
  combMod,
  factTable
) where

import Data.List
import qualified Data.Array.IArray as A

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
      | p > isqrt n = Just (m, (1, pps))
      | otherwise = let (q, r) = divMod m p in if r == 0 then Just (p, (q, pps)) else go (m, ps)

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . (fromIntegral :: Integral t => t -> Double)

comb :: Integral a => a -> a -> a
comb n r = div (fact n) (fact r * fact (n - r))
  where
    fact x = product [1 .. x]

modulus :: Integer
modulus = 1000000007

(+%) :: Integer -> Integer -> Integer
(+%) x y = mod (x + y) modulus

(*%) :: Integer -> Integer -> Integer
(*%) x y = mod (x * y) modulus

powMod :: Integer -> Integer -> Integer
powMod x n
  | n == 0 = 1
  | odd n = x *% powMod x (n-1)
  | otherwise = let y = powMod x (div n 2) in y *% y

type FactTable = A.Array Int (Integer, Integer)

combMod :: FactTable -> Integer -> Integer -> Integer
combMod t n r
  | n < r = 0
  | otherwise = fst (t ! n) *% snd (t ! r) *% snd (t ! (n - r))

factTable :: Int -> FactTable
factTable n = A.listArray (0, n) (zip facts factInvs)
  where
    facts = scanl (*%) 1 [1 .. fromIntegral n]
    factInvs = map (\x -> mod (powMod x (modulus - 2)) modulus) facts

(!) :: FactTable -> Integer -> (Integer, Integer)
(!) t i = t A.! fromIntegral i
