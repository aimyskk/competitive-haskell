module Math.Mod where

import qualified Data.Array.IArray as A

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

combMod :: FactTable -> Integer -> Integer -> Integer
combMod t n r
  | n < r = 0
  | otherwise = fst (t ! n) *% snd (t ! r) *% snd (t ! (n - r))
  where
    (!) arr i = arr A.! fromIntegral i

type FactTable = A.Array Int (Integer, Integer)

factTable :: Int -> FactTable
factTable n = A.listArray (0, n) (zip facts factInvs)
  where
    facts = scanl (*%) 1 [1 .. fromIntegral n]
    factInvs = map (\x -> mod (powMod x (modulus - 2)) modulus) facts
