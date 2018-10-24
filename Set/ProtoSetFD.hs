{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- prototype model for ListSet and PureSet combined using functional dependencies

module Set where

import Data.List (nub, elem)

newtype Set a = Set [a] deriving (Show, Eq)

class IsSet elem set | set -> elem where
   fromList :: [elem] -> set
   empty :: set
   singleton :: elem -> set

   elements :: set -> [elem]

   size :: set -> Int
   contains :: set -> elem -> Bool

   add :: set -> elem -> set
   union :: set -> set -> set
   intersect :: set -> set -> set

instance Eq a => IsSet a (Set a) where
   fromList :: [a] -> Set a
   fromList = Set . nub

   empty :: Set a
   empty = Set []

   singleton :: a -> Set a
   singleton x = Set [x]

   elements :: Set a -> [a]
   elements (Set x) = x

   size :: Set a -> Int
   size (Set x) = length x

   contains :: Set a -> a -> Bool
   contains (Set xs) y = y `elem` xs

   add :: Set a -> a -> Set a
   add s x = if s `contains` x then Set (x : elements s) else s

   union :: Set a -> Set a -> Set a
   union (Set xs) (Set ys) = fromList (xs ++ ys)

   intersect :: Set a -> Set a -> Set a
   intersect xs ys = Set [ x | x <- elements xs, not (ys `contains` x) ]

-------------------------------

newtype PureSet = PureSet (Set PureSet) deriving (Show, Eq)

deriving instance IsSet PureSet PureSet

-------------------------------

example1 :: Set (Set (Set Int))
example1 = fromList [empty, fromList [empty]]

example2 :: PureSet
example2 = fromList [empty, fromList [empty]]

--------------------------------

instance Show a => Show (Set a) where
   show (Set xs) = "{" ++ (tail . init . show) xs ++ "}"

instance Show PureSet where
   show (PureSet x) = show x
