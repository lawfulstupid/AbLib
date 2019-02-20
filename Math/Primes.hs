{-# LANGUAGE LambdaCase #-}

module Math.Primes where

import Data.Ratio
import AbLib.Data.List (expSearch, expSearchWeak)

-- sieved by eratosthenes himself
primes :: [Integer]
primes = sieve [2..] where
   sieve :: [Integer] -> [Integer]
   sieve (p:xs) = p : sieve [ x | x <- xs, mod x p /= 0 ]

-- Checks if a number is prime
isPrime :: Integer -> Bool
isPrime n = expSearch n primes
   
-- List all the prime factors of the argument
-- Such that `(product . factorise)` == `id`
factorise :: Integer -> [Integer]
factorise 1 = []
factorise n = case divisors n of
   []    -> [n]
   (p:_) -> p : factorise (div n p)
   where
   -- List the small prime divisors of the argument
   divisors :: Integer -> [Integer]
   divisors n = filter ((0==) . mod n) . primesUnder
      . floor . sqrt $ fromInteger n


-- Lists the primes less than or equal to the argument
primesUnder :: Integer -> [Integer]
primesUnder n = take k primes
   where k = maybe 0 (+1) (expSearchWeak n primes)
          
-- primesUnder :: Integer -> [Integer]
-- primesUnder n = aux primes where
   -- aux :: [Integer] -> [Integer]
   -- aux (p:ps) = case compare p n of
      -- LT -> p : aux ps
      -- EQ -> [p]
      -- GT -> []
                   

-- prime counting function: number of primes <= n
pc :: Integer -> Int
pc = length . primesUnder


-- multiplicative order of n modulo r
-- find least k > 0 such that n^k == 1 (mod r)
ord :: Integer -> Integer -> Integer
ord n a
   | gcd n a /= 1 = -1
   | otherwise = aux a where
   aux p = if mod p n == 1
      then 1
      else 1 + aux (a*p)
-- ord n a = aux $ map (\k -> mod (a^k) n) [1..] where
   -- aux (1:ks) = 1
   -- aux (_:ks) = 1 + aux ks

-- Euler's totient function
phi :: Integer -> Int
phi n = length $ filter ((==) n . denominator . flip (%) n) [1..n]
