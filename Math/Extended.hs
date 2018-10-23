{-# LANGUAGE LambdaCase #-}

module Math.Extended (
   Extended (NegInf, Real, PosInf),
   isFinite
) where

data Extended a = NegInf | Real {real :: a} | PosInf
   deriving (Eq, Ord)

isFinite :: Extended a -> Bool
isFinite (Real _) = True
isFinite (infval) = False

instance Functor Extended where
   fmap f (Real x) = Real $ f x
   fmap _ (NegInf) = NegInf
   fmap _ (PosInf) = PosInf
   
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
   (+) (Real x) = \case { Real y -> Real (x + y); i -> i + Real x }
   (+) (NegInf) = \case { PosInf -> undefined;    _ -> NegInf }
   (+) (PosInf) = \case { NegInf -> undefined;    _ -> PosInf }
   
   Real x * y = if isFinite y then fmap (x *) y else y * Real x
   NegInf * y = negate (PosInf * y)
   PosInf * y = case signum y of
      Real (-1) -> NegInf
      Real ( 0) -> undefined
      Real ( 1) -> PosInf
   
   negate (Real x) = Real $ negate x
   negate (NegInf) = PosInf
   negate (PosInf) = NegInf
   
   abs (Real x) = Real $ abs x
   abs (infval) = PosInf
   
   signum (Real x) = Real $ signum x
   signum (NegInf) = Real (-1)
   signum (PosInf) = Real ( 1)
   
   fromInteger n = Real $ fromInteger n
   
instance (Fractional a, Eq a) => Fractional (Extended a) where
   fromRational = Real . fromRational
   recip x = if isFinite x then fmap recip x else Real 0

nan :: Floating a => Extended a
nan = Real $ asin 2

instance (Floating a, Eq a) => Floating (Extended a) where
   pi      = Real pi
   exp     = \case { NegInf -> Real 0; x -> fmap exp x  }
   log     = \case { NegInf -> nan;    x -> fmap log x  }
   sqrt    = \case { NegInf -> nan;    x -> fmap sqrt x }
   sin x   = if isFinite x then fmap sin   x else nan
   cos x   = if isFinite x then fmap cos   x else nan
   tan x   = if isFinite x then fmap tan   x else nan
   asin x  = if isFinite x then fmap asin  x else nan
   acos x  = if isFinite x then fmap acos  x else nan
   atan x  = if isFinite x then fmap atan  x else (signum x) * pi / (fromInteger 2)
   sinh    = fmap sinh
   asinh   = fmap asinh
   cosh    = \case { NegInf -> PosInf; x -> fmap cosh x  }
   acosh   = \case { NegInf -> nan;    x -> fmap acosh x }
   tanh x  = if isFinite x then fmap tanh  x else signum x
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
   quotRem (Real n) (infval) = (Real 0, Real n)
   quotRem (infval) (Real _) = (infval, Real 0)
   quotRem _ _ = error "Infinity divided by Infinity"
   
instance Bounded (Extended a) where
   minBound = NegInf
   maxBound = PosInf

instance Show a => Show (Extended a) where
   show (Real x) = show x
   show (PosInf) = "+infinity"
   show (NegInf) = "-infinity"

