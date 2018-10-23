module Math.Natural where

import Data.Ratio ((%))

data Nat = Zero | Succ Nat deriving (Eq, Ord)

instance Show Nat where
   show = show . toInteger

instance Num Nat where
   n + Zero   = n
   n + Succ m = Succ n + m
   
   n      - Zero   = n
   Succ n - Succ m = n - m
   
   _ * Zero   = Zero
   n * Succ m = n * m + n
   
   abs = id
   
   signum Zero = Zero
   signum _    = Succ Zero
   
   fromInteger 0 = Zero
   fromInteger n = Succ $ fromInteger (n-1)
   
instance Real Nat where
   toRational = (% 1) . toInteger

instance Enum Nat where
   toEnum   = fromInteger . toInteger
   fromEnum = fromInteger . toInteger
   
   succ = Succ
   pred (Succ n) = n

instance Integral Nat where
   quotRem a b | a < b = (0, a)
   quotRem a b = (Succ q, r) where (q, r) = quotRem (a - b) b
   
   toInteger Zero = 0
   toInteger (Succ n) = (toInteger n) + 1