{-# LANGUAGE LambdaCase #-}

module ZFNum where
import Data.Ratio (numerator, denominator)

{-- NATURALS --}
data ZFNat = Zero | Succ ZFNat deriving (Eq, Ord)

instance Show ZFNat where
   show Zero     = "0"
   show (Succ n) = show n ++ "+"

instance Num ZFNat where
   n + 0 = n
   n + Succ m = Succ n + m
   n - 0 = n
   Succ n - Succ m = n - m
   n * 0 = 0
   n * Succ m = n + (n * m)
   abs = id
   signum 0 = 0
   signum _ = 1
   fromInteger 0 = Zero
   fromInteger n = Succ $ fromInteger (n - 1)
   
instance Enum ZFNat where
   succ = Succ
   toEnum = fromInteger . toInteger
   fromEnum Zero = 0
   fromEnum (Succ n) = 1 + fromEnum n

instance Real ZFNat where
   toRational = toRational . fromEnum
   
instance Integral ZFNat where
   quotRem n m = if n < m
      then (0, n)
      else let
         (q, r) = quotRem (n-m) m
         in (succ q, r)
   toInteger 0 = 0
   toInteger (Succ n) = 1 + toInteger n

{-- INTEGERS --}

newtype ZFInt = ZFInt (ZFNat, ZFNat) deriving (Show)

instance Eq ZFInt where
   ZFInt (a,b) == ZFInt (c,d) = a + d == c + b

instance Ord ZFInt where
   ZFInt (a,b) <= ZFInt (c,d) = a + d <= c + b
   
instance Num ZFInt where
   ZFInt (a,b) + ZFInt (c,d) = ZFInt (a+c, b+d)
   ZFInt (a,b) * ZFInt (c,d) = ZFInt (a*c+b*d, a*d+b*c)
   negate (ZFInt (a,b)) = ZFInt (b,a)
   abs (ZFInt (a,b)) = ZFInt (max a b, min a b)
   signum = \case {GT -> -1; EQ -> 0; LT -> 1} . compare 0
   fromInteger n = if n >= 0
      then ZFInt (fromInteger n, 0)
      else ZFInt (0, fromInteger n)
      

      
{-- RATIONALS --}

newtype ZFQuo = ZFQuo (ZFInt, ZFInt) deriving (Show)

instance Eq ZFQuo where
   ZFQuo (a,b) == ZFQuo (c,d) = a * d == c * b
   
instance Ord ZFQuo where
   ZFQuo (a,b) <= ZFQuo (c,d) = a * d <= c * b

instance Num ZFQuo where
   ZFQuo (a,b) + ZFQuo (c,d) = ZFQuo (a*d+c*d, b*d)
   ZFQuo (a,b) * ZFQuo (c,d) = ZFQuo (a*c, b*d)
   negate (ZFQuo (a,b)) = ZFQuo (negate a, b)
   abs (ZFQuo (a,b)) = ZFQuo (abs a, abs b)
   signum (ZFQuo (a,_)) = ZFQuo (signum a, 1)
   fromInteger n = ZFQuo (fromInteger n, 1)
   
instance Fractional ZFQuo where
   recip (ZFQuo (a,b)) = ZFQuo (b,a)
   fromRational q = ZFQuo (fromInteger $ numerator q, fromInteger $ denominator q)














































