
module AbLib.Math.Base where

import Data.Fixed

-- Represents a number in a given base
-- Intended for Num types
-- Param 1: base used
-- Param 2: digits in ascending order
data Base a = Base a [Int]

instance Show (Base a) where
   show (Base _ x) = reverse $ map toChar x where
      toChar :: Int -> Char
      toChar n
         |  0 <= n && n <=  9 = toEnum (48 + n)
         | 10 <= n && n <= 35 = toEnum (55 + n)
         | 36 <= n && n <= 61 = toEnum (61 + n)
         | otherwise = toEnum (61 + n) -- ?

-- cascade :: Real a => a -> [a] -> [Int]
cascade b []       = []
cascade b [0]      = []
cascade b (x:[])   = cascade b (x:0:[])
cascade b (x:y:xs) = let
   (q,r) = divMod' x b
   in r : cascade b ((y + fromIntegral q):xs)


-- Real a, Integral b =>
-- q :: b
-- r :: a
-- x :: a
-- b :: a

-- toBase b x = Base b $ cascade b [x]


{-
   b =  1.4
   x = 10
   1.4^6 <= 10 < 1.4^7
   k =  7
   
   01000100.01000000000000000000000010
   r[7] = 0.9486450616422
   r[6] = 0.32810308629908
   r[5] = 0.45934432081871
   r[4] = 0.64308204914619
   r[3] = 0.90031486880467
   r[2] = 0.26044081632654
   r[1] = 0.36461714285716
   r[0] = 0.51046400000002
-}
