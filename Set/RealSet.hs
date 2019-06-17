{-# LANGUAGE LambdaCase, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- Unfinished set type for storing ranges of
-- dense values such as reals and rationals.

module AbLib.Set.RealSet where

import AbLib.Set.Set
import AbLib.Data.Tuple
import AbLib.Control.Safe
import Data.List (sortOn, sortBy, nub)

newtype RealSet a = Union [Interval a]
data Interval a = Interval
   { lower :: Bound a
   , upper :: Bound a } deriving Eq
data Bound a = Bound
   { value :: a
   , btype :: BoundType } deriving Eq
data BoundType = OPEN | CLOSED deriving Eq

-- Construct a simple interval
interval :: Ord a => a -> BoundType -> a -> BoundType -> RealSet a
interval a ta b tb = Union $
   if a < b || (a == b && ta == CLOSED && tb == CLOSED)
   then [Interval (Bound a ta) (Bound b tb)] else []

intervals :: RealSet a -> [Interval a]
intervals (Union set) = set

-- Creates a comparator for Bounds by specifying which BoundType is "lesser"
boundOrd :: Ord a => BoundType -> Bound a -> Bound a -> Ordering
boundOrd t (Bound x tx) (Bound y ty) = case compare x y of
   EQ -> case (t ==) $# (tx, ty) of    -- values tied; refer to BoundTypes
      (True, False) -> LT              -- difference in BoundType: use supplied tiebreaker t
      (False, True) -> GT
      _             -> EQ              -- arguments are totally equal
   cp -> cp                            -- default: compare by value

instance Show a => Show (RealSet a) where
   show (Union []) = "{}"
   show (Union xs) = foldr1 (\ x y -> x ++ "+" ++ y) $ map showIntvl xs
      where
      showIntvl :: Show a => Interval a -> String
      showIntvl (Interval a b) = let
         parenA = if btype a == OPEN then "(" else "["
         parenB = if btype b == OPEN then ")" else "]"
         in parenA ++ show (value a) ++ "," ++ show (value b) ++ parenB

instance Ord a => Eq (RealSet a) where
   Union x == Union y = f x == f y
      where
      f = sortOn (\i -> value $# (lower,upper) #$ i)

instance (Ord a, Fractional a) => Set (RealSet a) a where
   empty = Union []
   singleton e = interval e CLOSED e CLOSED
   fromList xs = Union $ map (head . intervals . singleton) $ nub xs

   element (Union set) = do
      Interval a b <- safe head set
      return ((value a + value b) / 2)















