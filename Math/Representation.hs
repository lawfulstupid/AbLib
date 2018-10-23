{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Math.Representation where

import Math.PureSet
import Math.Natural
import Data.Ratio (numerator, denominator)

class Represents a b where
   encode :: a -> b
   
instance Represents a a where
   encode = id
   
{- Set-theoretic representation of naturals -}
instance Represents Nat     PureSet where
   encode = nat

instance Represents Word    PureSet where
   encode = nat
   
{- Set-theoretic representation of integers -}
instance Represents Integer PureSet where
   encode n = let
      a = div (abs n + n) 2
      b = div (abs n - n) 2
      in encode (nat a, nat b)
      
instance Represents Int     PureSet where
   encode = encode . toInteger
   
{- Set-theoretic representation of quotients -}
instance Represents Rational PureSet where
   encode q = encode (numerator q, denominator q)
   
instance Represents Float PureSet where
   encode = encode . toRational
   
instance Represents Double PureSet where
   encode = encode . toRational


instance (Represents a PureSet, Represents b PureSet) => Represents (a,b) PureSet where
   encode (x,y) = let
      a = encode x
      b = encode y
      in set [singleton a, set [a, b]]
