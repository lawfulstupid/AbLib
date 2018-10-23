module Expression where
import Data.Ratio (numerator, denominator)

data Op = Add | Sub | Mul | Div | Pwr
data Expr a = Con a
            | Var String
            | Opr Op (Expr a) (Expr a)
            | Fun String (Expr a)

instance Functor Expr where
   fmap f (Con x)      = Con (f x)
   fmap f (Opr op x y) = Opr op (fmap f x) (fmap f y)
   fmap f (Fun g x)    = Fun g (fmap f x)
   
instance Applicative Expr where
   pure = Con
   Con f <*> Con x = Con $ f x
   
instance Monad Expr where
   Con x >>= f = f x
   Var x >>= f = Var x
   Opr op x y >>= f = Opr op (x >>= f) (y >>= f)
   
instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"
   show Pwr = "^"

instance Show a => Show (Expr a) where
   show (Con c)      = show c
   show (Var x)      = x
   show (Opr op x y) = "(" ++ show x ++ show op ++ show y ++ ")"
   show (Fun f t)    = f ++ "(" ++ show t ++ ")"

instance Eq a => Eq (Expr a) where
   Con a == Con b = a == b
   Var x == Var y = x == y

instance (Eq a, Num a) => Num (Expr a) where
   Con x + Con y = Con (x + y)
   Con 0 + x     = x
   x     + Con 0 = x
   x     + y     = Opr Add x y
   Con x * Con y = Con (x * y)
   Con 1 * x     = x
   Con 0 * x     = Con 0
   x     * Con 1 = x
   x     * Con 0 = Con 0
   x     * y     = Opr Mul x y
   negate (Con x) = Con $ negate x
   negate (x)     = Opr Sub (Con 0) x
   abs (Con x) = Con $ abs x
   signum (Con x) = Con $ signum x
   fromInteger n = Con $ fromInteger n
   
instance (Eq a, Fractional a) => Fractional (Expr a) where
   Con x / Con y = Con (x / y)
   x     / Con 1 = x
   Con 0 / x     = Con 0
   x     / y     = Opr Div x y
   fromRational q = let
      a = Con $ fromInteger $ numerator q
      b = Con $ fromInteger $ denominator q
      in a / b
   
reduce :: (Eq a, Fractional a, Num a) => Expr a -> Expr a
reduce (Opr op x y) = let
   f = case op of {Add -> (+); Sub -> (-); Mul -> (*); Div -> (/); Pwr -> (Opr Pwr)}
   in f x y
reduce x = x