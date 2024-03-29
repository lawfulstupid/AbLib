module AbLib.Control.Monad where

import Control.Applicative

remonad :: (Foldable t, Alternative m) => t a -> m a
remonad = foldr (\x m -> pure x <|> m) empty

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g
infixr 9 <.>

liftU2 :: Alternative f => (a -> a -> a) -> f a -> f a -> f a
liftU2 f x y = liftA2 f x y <|> x <|> y
