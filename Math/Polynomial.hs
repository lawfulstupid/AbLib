{-# LANGUAGE GADTs, LambdaCase #-}


module Polynomial where
import Utilities (zipDef, delim)
-- data P a = P {x :: [a]} deriving (Eq, Show)

data Polynomial a where
   Poly :: [a] -> Polynomial a
   PAbs :: (a -> a) -> Polynomial a
   PSgn :: (a -> a) -> Polynomial a
   
coeffs :: Polynomial a -> [a]
coeffs (Poly p) = p
   
deg :: (Num a, Eq a) => Polynomial a -> Int
deg (Poly p) = aux $ reverse p where
   aux :: (Num a, Eq a) => [a] -> Int
   aux (0:l) = aux l
   aux l = length l - 1

(#) :: (Num a) => Polynomial a -> Int -> a
Poly p # i = if i < length p then p!!i else 0

polyZip :: (Num a, Num b) => (a -> b -> c) -> Polynomial a -> Polynomial b -> Polynomial c
polyZip f (Poly p) (Poly q) = Poly $ zip0 f p q
   
zip0 :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
zip0 = flip zipDef (0,0)

eval :: (Num a) => Polynomial a -> a -> a
eval (Poly p) x = sum $ zipWith (*) p $ map (x^) [0..]
eval (PAbs f) x = f x
eval (PSgn f) x = f x

-- multiply the polynomial by x^n, discard negative exponent terms
shift :: (Num a) => Int -> Polynomial a -> Polynomial a
shift n (Poly p)
   | n >= 0    = Poly $ [0 | i <- [1..n]] ++ p
   | otherwise = Poly $ drop (-n) p

diff :: (Num a, Enum a) => Polynomial a -> Polynomial a
diff (Poly p) = Poly $ tail $ zipWith (*) p [0..]

instance (Num a, Eq a) => Eq (Polynomial a) where
   Poly p == Poly q = and $ zip0 (==) p q
   
instance (Num a, Ord a) => Ord (Polynomial a) where
   Poly p <= Poly q = let (p', q') = unzip $ zip0 (,) p q
                      in reverse p' <= reverse q'

instance (Num a) => Num (Polynomial a) where
   p + q = polyZip (+) p q
   
   p * q = let n = 1 + (maximum $ map length [p, q])
               d = \i -> filter (\(s,t) -> s < t && t < n) $ map (\s -> (s, i-s)) [0..i]
               a = \i -> p#i * q#i
               b = \(s,t) -> (p#s + p#t) * (q#s + q#t)
               c2 = \i -> sum $ map b (d i)
               c1 = \i -> sum $ map (\(s,t) -> a s + a t) (d i)
               c0 = \i -> if even i then a (div i 2) else 0
               c = \i -> (c2 i) - (c1 i) + (c0 i)
           in Poly $ [a 0] ++ map c [1..2*n-3] ++ [a $ n-1]
           
   negate p = fmap negate p
   
   fromInteger = pure . fromInteger
   
   abs p = PAbs (abs . eval p)
   signum p = PSgn (signum . eval p)

instance Functor Polynomial where
-- fmap :: (a -> b) -> Polynomial a -> Polynomial b
   fmap f (Poly p) = Poly $ map f p

instance Applicative Polynomial where
-- pure :: a -> Polynomial a
   pure n = Poly [n]
-- (<*>) :: Polynomial (a -> b) -> Polynomial a -> Polynomial b
   Poly f <*> Poly p = Poly (f <*> p)
   
instance Monad Polynomial where
-- (>>=) :: Polynomial a -> (a -> Polynomial b) -> Polynomial b
   Poly p >>= f = Poly (p >>= coeffs . f)
   
instance Monoid (Polynomial a) where
-- mempty :: Polynomial a
   mempty = Poly []
-- mappend :: Polynomial a -> Polynomial a -> Polynomial a
   mappend (Poly p) (Poly q) = Poly (mappend p q)
   
instance Foldable Polynomial where
-- foldr :: (a -> b -> b) -> b -> Polynomial a -> b
   foldr f b (Poly p) = foldr f b p

instance Traversable Polynomial where
-- sequenceA :: Applicative f => Polynomial (f a) -> f (Polynomial a)
   sequenceA (Poly p) = fmap Poly (sequenceA p)

instance (Num a, Ord a, Show a) => Show (Polynomial a) where
-- show :: Polynomial a -> String
   show (Poly p) = let s = foldr (++) "" $ zipWith format p [0..]
                   in if length s == 0 then "0" else
                      if head s == '+' then tail s else s
                   where
      format :: (Show a, Ord a, Num a) => a -> Int -> String
      format 0 _ = ""
      format c 0 = (if c < 0 then "" else "+") ++ show c
      format 1 n = "+" ++ fpwr n
      format (-1) n = "-" ++ fpwr n
      format c n = format c 0 ++ fpwr n
      fpwr :: Int -> String
      fpwr 1 = "x"
      fpwr n = "x^" ++ show n

instance (Num a, Read a) => Read (Polynomial a) where
-- type ReadS (Polynomial a) = String -> [(Polynomial a, String)]
-- readsPrec :: Int -> ReadS (Polynomial a)
   readsPrec n str = let str' = filter (/= ' ') str
                         terms = filter (not . null) $ delim " " (str' >>= format)
                         pairs = map readTerm terms
                         p = sum $ map (\(c,n) -> shift n $ Poly [c]) pairs
                     in [(p, "")] where
      format :: Char -> String
      format '*' = ""
      format '^' = ""
      format '+' = " "
      format '-' = " -"
      format  c  = [c]
      readTerm :: (Read a) => String -> (a, Int)
      readTerm s = let (c,n) = full $ delim "x" s
                   in (read c, read n)
      full :: [String] -> (String, String)
      full [c]    = full [c,"0"]
      full [c,""] = full [c,"1"]
      full ["",n] = full ["1",n]
      full ["-",n] = full ["-1",n]
      full [c,n]  = (c, n)


 

-- type ListParser a = String -> [([a], String)]
-- parseList :: (Read a) => ListParser a
-- parseList str = aux [([], str)] where
   -- aux :: (Read a) => [([a], String)] -> [([a], String)]
   -- aux [] = []
   -- aux l = filter done l ++ aux (filter (not.done) l >>= f)
   -- done :: (a, String) -> Bool
   -- done (_,s) = null s
   -- f :: Read a => ([a], String) -> [([a], String)]
   -- f (l,s) = [(y:l, r) | (y,r) <- readsPrec 4 s]
   
   








