{-# LANGUAGE FunctionalDependencies #-}

-- Classes for defining behaviour of set types

module AbLib.Set.Set where

class Set s a | s -> a where
   -- The empty set.
   empty          :: s
   -- Construct a set containing a single given value.
   singleton      :: a -> s
   -- Construct a set from a list of elements.
   fromList       :: Eq a => [a] -> s
   -- Get an arbitrary element from this set.
   element        :: s -> Maybe a

   -- Check if a set is empty
   isEmpty        :: s -> Bool
   -- Check if a set contains given value.
   contains       :: Eq a => s -> a -> Bool
   -- Check is one set is contained by another.
   subset         :: Eq a => s -> s -> Bool
   -- Check if two sets contain no common elements.
   distinct       :: Eq a => s -> s -> Bool

   -- Add an element to a set, if it isn't already a member.
   add            :: Eq a => s -> a -> s
   -- Remove an element from a set, if it's a member.
   remove         :: Eq a => s -> a -> s
   
   -- Compute the union of two sets.
   union          :: Eq a => s -> s -> s
   unions         :: Eq a => [s] -> s
   unions = foldr union empty
   -- Compute the intersection of two sets.
   intersect      :: Eq a => s -> s -> s
   -- Compute the difference between two sets.
   complement     :: Eq a => s -> s -> s
   
   -- Compute the infimum and supermum of a set
   infsup         :: Ord a => s -> Maybe (a,a)
   
   -- Compute the supremum of a set
   sup            :: Ord a => s -> Maybe a
   sup = fmap snd . infsup
   -- Compute the infimum of a set
   inf            :: Ord a => s -> Maybe a
   inf = fmap fst . infsup

   -- Lift set values to a higher type, such as AbLib.Data.Extended
   lift           :: (Applicative f, Set t (f a)) => s -> t

class Set s a => DiscreteSet s a where
   -- Return a list of all the elements this set contains.
   elements       :: s -> [a]
   -- Compute the size of the set.
   size           :: s -> Int
   size           = length . elements

   -- Apply a map to elements of a set.
   smap           :: (DiscreteSet t b, Eq b) => (a -> b) -> s -> t
   -- Apply a filter to elements of a set.
   sfilter        :: (a -> Bool) -> s -> s
