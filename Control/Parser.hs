import AbLib.Data.Tuple
  
module Ablib.Control.Parser where

data Parser a = P (String -> [(a, String)])

instance Functor Parser where
   fmap f (P p) = P . map (f #$) . p

instance Applicative Parser where
   pure x = P $ \s -> [(x, s?)]
   P fs <*> P p = P $ \s -> [ (f x, r) | (x,r) <- p s, (f,_) <- fs s ]

composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

homomorphism
pure f <*> pure x = pure (f x)
pure f <*> pure x = pure (f x)

interchange
P fs <*> pure y = pure ($ y) <*> P fs
P fs <*> P (\s -> [(y,s?)])
 = P $ \s -> [ (f y, s?) | (f,_) <- fs s ]
 
pure ($ y) <*> P fs
 = P (\s -> [($ y, s?)]) <*> P fs
 = P $ \s -> [ (f y, r) | (f,r) <- fs s ]






