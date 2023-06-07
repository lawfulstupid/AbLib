module AbLib.Math.Modular (Modular, modulo, residue) where

-- Always assume `Mod x n` is in reduced form.
-- All Modular constructors should reduce their result.
data Modular a = Mod a a | AnyMod a

residue :: Modular a -> a
residue (Mod x n) = x
residue (AnyMod x) = x
   
modulo :: Integral a => a -> a -> Modular a
x `modulo` n = reduce $ Mod x n

reduce :: Integral a => Modular a -> Modular a
reduce (Mod x n) = Mod (x `mod` n) n
reduce (AnyMod x) = AnyMod x

instance Show a => Show (Modular a) where
   show (Mod x n) = show x

instance Integral a => Eq (Modular a) where
   Mod x n == Mod y m = n == m && x == y
   Mod x n == AnyMod y = Mod x n == y `modulo` n
   AnyMod x == Mod y m = x `modulo` m == Mod y m
   AnyMod x == AnyMod y = x == y

instance Integral a => Ord (Modular a) where
   Mod x n <= Mod y m = n == m && x <= y
   Mod x n <= AnyMod y = Mod x n <= y `modulo` n
   AnyMod x <= Mod y m = x `modulo` m <= Mod y m
   AnyMod x <= AnyMod y = x <= y

instance Enum a => Enum (Modular a) where
   toEnum n = AnyMod (toEnum n)
   fromEnum (Mod x n) = fromEnum x
   fromEnum (AnyMod x) = fromEnum x

instance Integral a => Num (Modular a) where
   fromInteger n = AnyMod 0
   (+) = opIfEqual (+)
   (*) = opIfEqual (*)
   (-) = opIfEqual (-)
   negate (Mod x n) = reduce $ Mod (negate x) n
   negate (AnyMod x) = AnyMod (negate x)
   abs = id
   signum (Mod x n) = Mod (signum x) n
   signum (AnyMod x) = AnyMod (signum x)

opIfEqual :: Integral a => (a -> a -> a) -> Modular a -> Modular a -> Modular a
opIfEqual f (Mod x n) (Mod y m) = if n == m
   then reduce $ Mod (f x y) n
   else error "mismatching moduli"
opIfEqual f (Mod x n) (AnyMod y) = opIfEqual f (Mod x n) (y `modulo` n)
opIfEqual f (AnyMod x) (Mod y m) = opIfEqual f (x `modulo` m) (Mod y m)
opIfEqual f (AnyMod x) (AnyMod y) = AnyMod (f x y)

instance Integral a => Fractional (Modular a) where
   recip (Mod x n) = let (y,_) = gcde x n in y `modulo` n

gcde :: Integral a => a -> a -> (a, a)
gcde a b = aux a b 1 0 0 1
   where
   aux _ 0 x1 y1 _ _ = (x1, y1)
   aux a b x1 y1 x2 y2 = let
      (q,r) = divMod a b
      in aux b r x2 y2 (x1-q*x2) (y1 - q * y2)

-- Real, Integral, Fractional