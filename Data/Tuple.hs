module AbLib.Data.Tuple where

(##$) :: (a -> b, a -> c, a -> d) -> a -> (b, c, d)
(f,g,h) ##$ x = (f x, g x, h x)
infixr 0 ##$

(#$) :: (a -> b, a -> c) -> a -> (b, c)
(f,g) #$ x = (f x, g x)
infixr 0 #$

(#$#) :: (a0 -> a1, b0 -> b1) -> (a0, b0) -> (a1, b1)
(f,g) #$# (x,y) = (f x, g y)
infixr 0 #$#

($#) :: (a -> b) -> (a, a) -> (b, b)
f $# (x,y) = (f x, f y)
infixr 0 $#

($##) :: (a -> b) -> (a, a, a) -> (b, b, b)
f $## (x,y,z) = (f x, f y, f z)
infixr 0 $##

(##$##) :: (a0 -> a1, b0 -> b1, c0 -> c1) -> (a0, b0, c0) -> (a1, b1, c1)
(f,g,h) ##$## (x,y,z) = (f x, g y, h z)
infixr 0 ##$##

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

mfst :: (a -> c) -> (a, b) -> (c, b)
mfst f (x,y) = (f x, y)

msnd :: (b -> c) -> (a, b) -> (a, c)
msnd f (x,y) = (x, f y)