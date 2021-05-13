
module AbLib.Data.Seq where

--------------------------------------------------------------------------------

data Seq a = Empty | Seq a :+ a

--------------------------------------------------------------------------------

instance Show a => Show (Seq a) where
   show = show . toList

instance Semigroup (Seq a) where
   xs <> ys = fromList (toList xs <> toList ys)

instance Monoid (Seq a) where
   mempty = Empty

instance Functor Seq where
   fmap f Empty = Empty
   fmap f (xs :+ x) = fmap f xs :+ f x

instance Foldable Seq where
   foldMap f = foldMap f . toList

--------------------------------------------------------------------------------

fromList :: [a] -> Seq a
fromList [] = Empty
fromList xs = let (s,[x]) = splitAt (length xs - 1) xs in fromList s :+ x

toList :: Seq a -> [a]
toList Empty = []
toList (xs :+ x) = toList xs ++ [x]
