
module AbLib.Data.Indexed where

data Indexed a = Index
   { index :: Int
   , value :: a }

instance Functor Indexed where
   fmap f (Index n x) = Index n (f x)

mapIndex :: (Int -> Int) -> Indexed a -> Indexed a
mapIndex f (Index n x) = Index (f n) x

mergeIndices :: (Int -> Int -> Int) -> (a -> b -> c) -> Indexed a -> Indexed b -> Indexed c
mergeIndices f g (Index n x) (Index m y) = Index (f n m) (g x y)

increment :: Indexed a -> Indexed a
increment = mapIndex (+1)

decrement :: Indexed a -> Indexed a
decrement = mapIndex (flip (-) 1)
