
module AbLib.Data.Stack (
   Stack, fromList, toList,
   push, pop, peek, popn
) where

import AbLib.Data.Tuple
import Data.Maybe (listToMaybe)

--------------------------------------------------------------------------------

newtype Stack a = Stack [a]

--------------------------------------------------------------------------------

instance Show a => Show (Stack a) where
   show (Stack xs) = show xs

instance Semigroup (Stack a) where
   Stack xs <> Stack ys = Stack (xs <> ys)

instance Monoid (Stack a) where
   mempty = Stack []

instance Foldable Stack where
   foldMap f (Stack xs) = foldMap f xs

instance Functor Stack where
   fmap f (Stack xs) = Stack $ fmap f xs

--------------------------------------------------------------------------------

fromList :: [a] -> Stack a
fromList = Stack

toList :: Stack a -> [a]
toList (Stack xs) = xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop = mfst listToMaybe . popn 1

peek :: Stack a -> Maybe a
peek = fst . pop

popn :: Int -> Stack a -> ([a], Stack a)
popn n (Stack xs) = msnd Stack $ splitAt n xs
