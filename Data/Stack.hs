
module AbLib.Data.Stack (
   Stack, fromList, toList,
   push, pop, peek
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

--------------------------------------------------------------------------------

fromList :: [a] -> Stack a
fromList = Stack

toList :: Stack a -> [a]
toList (Stack xs) = xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack xs) = (listToMaybe, Stack) #$# splitAt 1 xs

peek :: Stack a -> Maybe a
peek = fst . pop
