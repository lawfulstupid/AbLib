{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- A set type for storing ranges of Ord values.
-- I've realised some problems with it, and am
-- reworking it into RealSet.
-- This was also created before AbLib so I'm not
-- entirely sure it works, not tested it.

-- TL;DR: deprecated

module Math.OrdSet (
   OrdSet, Interval, Bound, BoundType (Open, Closed), -- types
   empty, singleton, total, interval,                 -- construction
   isEmpty, nonEmpty, contains, subset, distinct,     -- predicates
   union, intersect, difference, complement,          -- operations
   infsup, supinf, sup, inf, range, lift              -- other
) where

import AbLib.Set.Set
import AbLib.Data.Tuple
import Data.List (sortOn, sortBy)
import GHC.Base (liftA2)

newtype OrdSet a = Union [Interval a]
type Interval a = (Bound a, Bound a)
type Bound a = (a, BoundType)
data BoundType = Open | Closed deriving (Eq)

intervals :: OrdSet a -> [Interval a]
intervals (Union set) = set

-- Creates a comparator by specifying which BoundType is "lesser"
boundOrd :: Ord a => BoundType -> Bound a -> Bound a -> Ordering
boundOrd t (x,tx) (y,ty) = case compare x y of
   EQ -> case (t ==) $# (tx, ty) of
      (True, False) -> LT
      (False, True) -> GT
      _             -> EQ
   cp -> cp
   
instance Functor OrdSet where
   fmap f (Union x) = Union $ map g x where
      g ((a,ta),(b,tb)) = ((f a, ta), (f b, tb))

instance Show a => Show (OrdSet a) where
   show (Union []) = "Ø"
   show (Union xs) = foldr1 (\x y -> x ++ "+" ++ y) $ map aux xs where
      aux :: Show a => Interval a -> String        -- show interval
      aux ((a,ta), (b,tb)) = let
         parenA = if ta == Open then "(" else "["  -- get appropriate brackets
         parenB = if tb == Open then ")" else "]"
         in parenA ++ show a ++ "," ++ show b ++ parenB
         
instance Ord a => Eq (OrdSet a) where
   Union x == Union y = f x == f y where
      f = sortOn (both fst)

instance Set (OrdSet a) a where
   empty = Union []
   singleton x = Union [( (x, Closed), (x, Closed) )]
   isEmpty = null . intervals
   
   contains (Union set) x = any aux set where   -- check if value in any subinterval
      aux ((a,ta),(b,tb)) = low && upp where
         low = a < x || x == a && ta == Closed  -- both of these are satisfied
         upp = x < b || x == b && tb == Closed  --    iff x is in this interval

   union sets = Union $ reduce   -- combine overlapping intervals, return
      $ sortOn (both fst)        -- sort by (inf,sup)
      $ foldMap intervals sets   -- make flat list of all intervals involved
    where
      reduce :: Ord a => [Interval a] -> [Interval a]         -- combines overlaps
      reduce [] = []
      reduce [x] = [x]
      reduce (x:y:is) = let
         (_, (b,tb)) = x
         ((a,ta), _) = y
         lo = head $ sortBy (boundOrd Closed) $ map fst [x,y] -- least lower bound
         hi = last $ sortBy (boundOrd Open)   $ map snd [x,y] -- greatest upper bound
         in if b < a || b == a && ta == Open && tb == Open    -- if x,y distinct
            then x : reduce (y:is)                            -- x done, union remainder
            else reduce ((lo,hi) : is)                        -- combine x,y, repeat

   intersect sets = if null sets
      then empty
      else foldr1 aux sets
    where
      aux :: Ord a => OrdSet a -> OrdSet a -> OrdSet a
      aux (Union xs) (Union ys) = union [int x y | x <- xs, y <- ys]
      int :: Ord a => Interval a -> Interval a -> OrdSet a    -- intersect intervals
      int x y = let
         a = last $ sortBy (boundOrd Closed) $ map fst [x,y]  -- greatest lower bound
         b = head $ sortBy (boundOrd Open)   $ map snd [x,y]  -- least upper bound
         in interval a b

   complement x s = let
      r@(Union [(lo,hi)]) = range $ union [x,s]
      s1 = (sortOn (fst . fst) $ intervals s) >>= \(a,b) -> [a,b]
      s2 = [lo] ++ map swap s1 ++ [hi]
      in intersect [x, union $ build s2]
    where
      build :: Ord a => [Bound a] -> [OrdSet a]
      build [] = []
      build (x:y:bs) = interval x y : build bs
      swap :: Bound a -> Bound a
      swap (x,Open) = (x,Closed)
      swap (x,Closed) = (x,Open)
   
   infsup (Union set) = case map (both fst) set of
      [] -> Nothing
      xs -> Just $ foldr1 aux xs where                -- fold into min/max
         aux :: Ord a => (a, a) -> (a, a) -> (a, a)
         aux (a,b) (c,d) = (min a c, max b d)         -- update min/max
      
range x | isEmpty x = empty
range x = Union [foldr1 f $ intervals x] where
   f :: Ord a => Interval a -> Interval a -> Interval a
   f (a,b) (c,d) = let
      x = head $ sortBy (boundOrd Closed) [a,c]
      y = last $ sortBy (boundOrd Open)   [b,d]
      in (x,y)

-- Constructs a simple interval given lower and upper bounds.
interval :: (Ord a) => Bound a -> Bound a -> OrdSet a
interval (a,ta) (b,tb) = Union $ 
   if a < b || (a == b && ta == Closed && tb == Closed)
   then [( (a,ta), (b,tb) )]
   else []
