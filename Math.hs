module Math (
  modulus,
  (+%),
  (*%),
  primes,
  factorization,
  isqrt,
  comb,
  fact,
  pow,
  combMod,
  factTable
) where

import Data.List
import qualified Data.Array.IArray as A

type FactTable = A.Array Int (Int,Int)

modulus :: Int
modulus = 1000000007

(+%) :: Int -> Int -> Int
(+%) x y = mod (x + y) modulus

(*%) :: Int -> Int -> Int
(*%) x y = fromIntegral $ mod (xz * yz) (fromIntegral modulus)
  where
    xz = fromIntegral x :: Integer
    yz = fromIntegral y :: Integer

primes :: Integral a => [a]
primes = 2 : 3 : [x | i <- [1..], j <- [-1,1], let x = 6*i+j, isPrime x]
  where
    isPrime n = null [i | i <- takeWhile (\p -> p*p <= n) primes, mod n i == 0]

factorization :: Integral a => a -> [a]
factorization n = unfoldr go (n, primes)
  where
    go (_, []) = Nothing
    go (m, pps@(p:ps))
      | m == 1 = Nothing
      | p > isqrt n = Just (m, (1, pps))
      | otherwise = let (q,r) = divMod m p in if r == 0 then Just (p, (q,pps)) else go (m,ps)

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . (fromIntegral :: Integral a => a -> Double)

comb :: Integral a => a -> a -> a
comb n r = div (fact n) (fact r * fact (n-r))

fact :: Integral a => a -> a
fact n = product [1..n]

pow :: Int -> Int -> Int
pow x n
  | n == 0 = 1
  | odd n = x *% pow x (n-1)
  | otherwise = mod (pow x (div n 2) ^ (2 :: Int)) modulus

combMod :: FactTable -> Int -> Int -> Int
combMod t n r
  | n < r = 0
  | otherwise = fst (t A.! n) *% snd (t A.! r) *% snd (t A.! (n-r))

factTable :: Int -> FactTable
factTable n = A.listArray (0, n) (zip facts factInvs)
  where
    facts = scanl (*%) 1 [1..n]
    factInvs = map (\x -> mod (pow x (modulus - 2)) modulus) facts
