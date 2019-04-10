
{- A list with an origin, extending both forwards and backwards. -}
module AbLib.Data.BiList where

import Control.Applicative
import Control.Monad

data BiList a = BiList [a] a [a] | Empty
   deriving Eq

instance Foldable BiList where
   foldMap _ Empty = mempty
   foldMap f bList = case fmap f bList of
      BiList l c r -> mconcat (reverse l ++ [c] ++ r)

instance Functor BiList where
   fmap _ Empty = Empty
   fmap f (BiList l c r) = BiList (map f l) (f c) (map f r)

instance Applicative BiList where
   pure x = BiList [] x []
   Empty <*> _ = Empty
   _ <*> Empty = Empty
   BiList f g h <*> BiList l c r = BiList (tail (g:f <*> c:l)) (g c) (tail (g:h <*> c:r))

instance Show a => Show (BiList a) where
   show Empty = "[]"
   show (BiList l c r) = let
      ls = init . tail . show $ reverse l
      cs = show c
      rs = init . tail $ show r
      ls' = if ls == "" then ls else ls ++ "|"
      rs' = if rs == "" then rs else "|" ++ rs
      in "[" ++ ls' ++ cs ++ rs' ++ "]"

fromList :: [a] -> BiList a
fromList [] = Empty
fromList (x:xs) = BiList [] x xs

fromListAt :: Int -> [a] -> BiList a
fromListAt n xs = BiList l c r where (l,c:r) = splitAt n xs