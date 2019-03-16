module AbLib.Data.Tuple where

-- 3 functions to 1 argument (apply to all)
(##$) :: (a -> b, a -> c, a -> d) -> a -> (b, c, d)
(f,g,h) ##$ x = (f x, g x, h x)
infixr 0 ##$

-- 2 function to 1 argument (apply to both)
(#$) :: (a -> b, a -> c) -> a -> (b, c)
(f,g) #$ x = (f x, g x)
infixr 0 #$

-- 2 functions to 2 arguments (apply in parallel)
(#$#) :: (a0 -> a1, b0 -> b1) -> (a0, b0) -> (a1, b1)
(f,g) #$# (x,y) = (f x, g y)
infixr 0 #$#

-- 1 function to 2 arguments (map both)
($#) :: (a -> b) -> (a, a) -> (b, b)
f $# (x,y) = (f x, f y)
infixr 0 $#

-- 1 function to 3 arguments (map all)
($##) :: (a -> b) -> (a, a, a) -> (b, b, b)
f $## (x,y,z) = (f x, f y, f z)
infixr 0 $##

-- 3 functions to 3 arguments (map all)
(##$##) :: (a0 -> a1, b0 -> b1, c0 -> c1) -> (a0, b0, c0) -> (a1, b1, c1)
(f,g,h) ##$## (x,y,z) = (f x, g y, h z)
infixr 0 ##$##

-- Flip a tuple
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- Map the first value
mfst :: (a -> c) -> (a, b) -> (c, b)
mfst f (x,y) = (f x, y)

-- Map the second value
msnd :: (b -> c) -> (a, b) -> (a, c)
msnd f (x,y) = (x, f y)

-- Replicates a value
two :: a -> (a,a)
two x = (x,x)