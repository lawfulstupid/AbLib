module AbLib.Control.Alias where

-- "then" operator. Composes functions left-to-right
($>) :: a -> (a -> b) -> b
($>) = flip ($)
infixl 0 $>

-- Performs monads in left-to-right sequence as normal,
-- but discards second value rather than first
(<<) :: Monad m => m a -> m b -> m a
f << g = do x <- f; g; return x
