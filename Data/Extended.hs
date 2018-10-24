{-# LANGUAGE LambdaCase #-}

-- Module for extending any well-ordered numerical type to include infinite values.

module AbLib.Math.Extended (
   Extended (..), isFinite
) where

data Extended a = NegInf | Real {real :: a} | PosInf
   deriving (Eq, Ord) -- Ord behaves really nicely here

instance Show a => Show (Extended a) where
   show (Real x) = show x
   show (PosInf) = "+" ++ infsym
   show (NegInf) = "-" ++ infsym

infsym :: String
infsym = "Infinity"     -- can change this to something nicer later

dontDoThis :: a
dontDoThis = undefined  -- debating whether to use undefined or throw proper exception

nan :: Floating a => Extended a
nan = Real $ asin 2     -- need a NaN value for Floating instance

isFinite :: Extended a -> Bool
isFinite (Real _) = True
isFinite (infval) = False

-- Doesn't always behave great, but you get the idea.
instance Functor Extended where
   fmap f (Real x) = Real $ f x
   fmap _ (NegInf) = NegInf
   fmap _ (PosInf) = PosInf
   
-- Not sure if Applicative instance will ever be useful, certainly doesn't make much sense.
instance Applicative Extended where
   pure = Real
   Real f <*> x = fmap f x
   NegInf <*> _ = NegInf
   PosInf <*> _ = PosInf
   
instance Monad Extended where
   Real x >>= f = f x
   NegInf >>= _ = NegInf
   PosInf >>= _ = PosInf

instance (Num a, Eq a) => Num (Extended a) where
   Real x + Real y = Real (x + y)
   Real _ + infval = infval
   PosInf + PosInf = PosInf
   NegInf + NegInf = NegInf
   PosInf + NegInf = dontDoThis
   x + y = y + x
   
   Real x * Real y = Real (x * y)
   x * y = case signum x * signum y of
      1 -> PosInf
      0 -> dontDoThis
      _ -> NegInf       -- I only used a hole to make this look pretty
   
   negate (Real x) = Real $ negate x
   negate (NegInf) = PosInf
   negate (PosInf) = NegInf
   
   abs (Real x) = Real $ abs x
   abs (infval) = PosInf
   
   signum (Real x) = Real $ signum x
   signum (NegInf) = -1
   signum (PosInf) =  1
   
   fromInteger n = Real $ fromInteger n
   
instance (Fractional a, Eq a) => Fractional (Extended a) where
   fromRational = Real . fromRational

   recip (Real x) = Real $ recip x
   recip ________ = 0

instance (Floating a, Eq a) => Floating (Extended a) where
   pi      = Real pi
   exp     = \case { NegInf -> 0;   x -> fmap exp x  }
   log     = \case { NegInf -> nan; x -> fmap log x  }
   sqrt    = \case { NegInf -> nan; x -> fmap sqrt x }
   sin  x  = if isFinite x then fmap sin  x else nan
   cos  x  = if isFinite x then fmap cos  x else nan
   tan  x  = if isFinite x then fmap tan  x else nan
   asin x  = if isFinite x then fmap asin x else nan
   acos x  = if isFinite x then fmap acos x else nan
   atan x  = if isFinite x then fmap atan x else (signum x) * pi / (fromInteger 2)
   sinh    = fmap sinh
   asinh   = fmap asinh
   cosh    = \case { NegInf -> PosInf; x -> fmap cosh x  }
   acosh   = \case { NegInf -> nan;    x -> fmap acosh x }
   tanh  x = if isFinite x then fmap tanh  x else signum x
   atanh x = if isFinite x then fmap atanh x else nan
   
instance Real a => Real (Extended a) where
   toRational (Real x) = toRational x
   toRational (infval) = error "Infinity has no Rational representation"
   
instance RealFrac a => RealFrac (Extended a) where
   properFraction (Real x) = fmap Real $ properFraction x
   properFraction (infval) = error "Infinity cannot be written as a proper fraction"
   
instance Enum a => Enum (Extended a) where
   toEnum = Real . toEnum
   fromEnum = fromEnum . real
   succ x = fmap succ x
   pred x = fmap pred x
   
instance Integral a => Integral (Extended a) where
   toInteger (Real n) = toInteger n
   toInteger (infval) = error "Infinity has no Integral representation"
   
   quotRem (Real n) (Real k) = (Real q, Real r) where (q,r) = quotRem n k
   quotRem (Real n) (infval) = (0, Real n)
   quotRem (infval) (Real n) = (infval * Real n, 0)
   quotRem _ _ = error "Diving Infinity by Infinity"
   
instance Bounded (Extended a) where
   minBound = NegInf
   maxBound = PosInf
