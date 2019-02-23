{-# LANGUAGE LambdaCase #-}

module Polynomial where

import AbLib.Data.List ((!?), zipDef, delim)
import AbLib.Data.Tuple ((#$#))
import Data.Char (isSpace)
import AbLib.Control.Alias (($>))
-- data P a = P {x :: [a]} deriving (Eq, Show)

newtype Polynomial a = Poly [a]

-- Produces the coefficients of a Polynomial as a list
coeffs :: Polynomial a -> [a]
coeffs (Poly p) = p

-- Applies a function inside the `Poly` wrapper
onCoeffs :: ([a] -> [b]) -> Polynomial a -> Polynomial b
onCoeffs f (Poly p) = Poly (f p)

-- Computes the degree of a Polynomial
-- deg(0) = -1
deg :: (Num a, Eq a) => Polynomial a -> Int
deg = (+(-1)) . length . dropWhile (==0) . reverse . coeffs

-- Gets the nth coefficient of a Polynomial
(#) :: (Num a) => Polynomial a -> Int -> a
Poly p # n = maybe 0 id $ p !? n

-- zip two Polynomials using a given combining function
polyZip :: (Num a, Num b) => (a -> b -> c) -> Polynomial a -> Polynomial b -> Polynomial c
polyZip f (Poly p) q = onCoeffs (zip0 f p) q

-- zip two lists of numbers until BOTH are depleted, using 0 as a default value if one is too short
zip0 :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
zip0 = flip zipDef (0,0)

-- evaluate a Polynomial on a given value
eval :: Num a => Polynomial a -> a -> a
eval (Poly p) x = sum $ zipWith (*) p [x^n | n <- [0..]]

-- multiply the Polynomial by x^n, discarding negative exponent terms
shift :: (Num a) => Int -> Polynomial a -> Polynomial a
shift n p = onCoeffs ((replicate n 0 ++) . drop (-n)) p

-- differentiate a Polynomial
diff :: (Num a, Enum a) => Polynomial a -> Polynomial a
diff = onCoeffs $ tail . zipWith (*) [0..]

---------- MATHEMATICAL INSTANCES ----------

instance (Num a, Eq a) => Eq (Polynomial a) where
   Poly p == Poly q = and $ zip0 (==) p q -- need to compare the zeroes too
   
instance (Num a, Ord a) => Ord (Polynomial a) where
   -- slightly arbitrary and slow comparator
   Poly p <= Poly q = let
      (p', q') = unzip $ zip0 (,) p q  -- p',q' have the same length but equal p,q respectively
      in reverse p' <= reverse q'      -- compare by most significant coefficient

-- don't want `signum` and `abs` instances, too much hassle
instance (Num a) => Num (Polynomial a) where
   fromInteger = pure . fromInteger
   negate = fmap negate
   (+) = polyZip (+)
   
   p * q = let -- I think this is the Karatsuba algorithm
      n    = 1 + (maximum $ map length [p,q])
      d  i = filter (\(s,t) -> s < t && t < n) $ map (\s -> (s, i-s)) [0..i]
      a  i = p#i * q#i
      b    = \(s,t) -> (p#s + p#t) * (q#s + q#t)
      c2 i = sum $ map b (d i)
      c1 i = sum $ map (\(s,t) -> a s + a t) (d i)
      c0 i = if even i then a (div i 2) else 0
      c  i = (c2 i) - (c1 i) + (c0 i)
      in Poly $ [a 0] ++ map c [1..2*n-3] ++ [a $ n-1]

---------- LANGUAGE INSTANCES ----------

instance Functor Polynomial where
-- fmap :: (a -> b) -> Polynomial a -> Polynomial b
   fmap f = onCoeffs $ map f

instance Applicative Polynomial where
-- pure :: a -> Polynomial a
   pure n = Poly [n]
-- (<*>) :: Polynomial (a -> b) -> Polynomial a -> Polynomial b
   Poly f <*> Poly p = Poly (f <*> p)
   
instance Monad Polynomial where
-- (>>=) :: Polynomial a -> (a -> Polynomial b) -> Polynomial b
   Poly p >>= f = Poly (p >>= coeffs . f)
   
instance Foldable Polynomial where
-- foldr :: (a -> b -> b) -> b -> Polynomial a -> b
   foldr f b (Poly p) = foldr f b p

instance Traversable Polynomial where
-- sequenceA :: Applicative f => Polynomial (f a) -> f (Polynomial a)
   sequenceA (Poly p) = fmap Poly (sequenceA p)

instance (Num a, Ord a, Show a) => Show (Polynomial a) where
-- show :: Polynomial a -> String
   show (Poly p) = case foldr (++) "" $ zipWith format p [0..] of
      ""      -> "0"    -- occurs when all coefficients are 0
      ('+':s) -> s      -- drop leading '+'
      s       -> s      -- oll korrect
      where
      
      format :: (Show a, Ord a, Num a) => a -> Int -> String
      format 0    _ = ""                     -- coefficient of 0 -> don't show this term
      format c    0 = lead c     ++ show c   -- constant coefficient -> just show value, add leading '+' if positive
      format 1    n = "+"        ++ fpwr n   -- unit coefficient -> ignore
      format (-1) n = "-"        ++ fpwr n   -- unit coefficient -> ignore
      format c    n = format c 0 ++ fpwr n   -- general case -> deal with coefficient then variable separately
      
      fpwr :: Int -> String                  -- handles how to show variable and exponent
      fpwr 1 = "x"                           -- unit exponent -> ignore
      fpwr n = "x^" ++ show n                -- general case  -> show normally
      
      lead :: (Ord a, Num a) => a -> String  -- create leading '+' symbol for positive coefficients
      lead c = if c < 0 then "" else "+"
   

instance (Num a, Read a) => Read (Polynomial a) where
-- readsPrec :: Int -> String -> [(Polynomial a, String)]
   readsPrec n str = str                     -- start with input
      $> filter (not . isSpace)              -- remove whitespace
      $> (>>= format)                        -- apply `format` function
      $> delim "+"                           -- split at plusses into list of terms (in String form)
      $> filter (not . null)                 -- remove empty Strings
      $> map readTerm                        -- interpret each term independently as a tuple
      $> map (\(c,n) -> shift n $ Poly [c])  -- interpret each tuple as a Polynomial
      $> sum                                 -- add these Polynomial terms together
      $> (,"") $> return                     -- return in correct format
      where
      
      format :: Char -> String
      format = \case { '*' -> ""; '^' -> ""; '-' -> "+-"; c -> [c] }
      -- we can use 'x' as a delimiter within each term, so we don't need to keep '*' and '^'
      -- '-' must be rewritten as "+-" in order to split terms at '+'
      -- everything else stays the same
      
      -- interprets a String "CxN" style poly term as a tuple
      readTerm :: (Read a) => String -> (a, Int)
      readTerm s = s
         $> delim "x"               -- split around the 'x', ideally get a list of two numbers (as Strings)
         $> full                    -- fix some edge cases to guarantee we have a tuple of numbers
         $> ((read,read) #$#)       -- read both into actual number types
      
      -- handles edge cases from delimiting a term
      full :: [String] -> (String, String)
      full [c]     = full [c,"0"]   -- if there was no 'x'
      full [c,""]  = full [c,"1"]   -- if there was nothing to the right of 'x'
      full ["",n]  = full ["1",n]   -- if there was nothing to the left of 'x'
      full ["-",n] = full ["-1",n]  -- if there was only '-' to the left of 'x'
      full [c,n]   = (c, n)         -- repackage as tuple


   
   








