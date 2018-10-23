{-# LANGUAGE LambdaCase #-}

module Math.Primes where

import Data.Ratio
import Utilities

-- sieved by eratosthenes himself
primes :: [Integer]
primes = aux [2..] where
   aux :: [Integer] -> [Integer]
   aux (p:ps) = p : (aux $ filter (\n -> mod n p /= 0) ps)

isPrime :: Integer -> Bool
isPrime n = expSearch n primes

divisors :: Integer -> [Integer]
divisors n = let max = (floor . sqrt . fromInteger) n
             in filter (\p -> mod n p == 0) $ primesunder max
   
-- factorise :: Integer -> [Integer]
-- factorise n = aux n (divisors n) where
   -- aux :: Integer -> [Integer] -> [Integer]
   -- aux n [] = [n]
   -- aux 1 _ = []
   -- aux n ops = let ops' = filter (\p -> mod n p == 0) ops
                   -- p = head ops'
               -- in  p : aux (div n p) ops'
factorise :: Integer -> [Integer]
factorise 1 = []
factorise n = case divisors n of
                 [] -> [n]
                 ps -> head ps : factorise (div n $ head ps)


-- primesunder :: Integer -> [Integer]
-- primesunder n = case x of
                   -- Just xs -> xs
                   -- Nothing -> []
                -- where
   -- x = do i <- expSearchWeak n primes
          -- return $ take (i+1) primes
          
primesunder :: Integer -> [Integer]
primesunder n = aux primes where
   aux :: [Integer] -> [Integer]
   aux (p:ps) = case compare p n of
                   LT -> p : aux ps
                   EQ -> [p]
                   GT -> []
                   

-- prime counting function: number of primes <= n
pc :: Integer -> Int
pc = length . primesunder


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
