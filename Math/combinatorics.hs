import Data.Ratio

-- factorial of n
fact :: Integer -> Integer
fact n = inj n n

-- number of injections f:[n]->[k]
inj :: Integer -> Integer -> Integer
inj n k = foldl (*) 1 [n-k+1..n]

-- # of k-subsets of an n-set
choose :: Integer -> Integer -> Integer
choose n k = div (inj n k) (fact k)

-- probability of a k-subset from an n-set with m targets containing p targets
select :: Integer -> Integer -> Integer -> Integer -> Rational
select n m k p = (choose (n-k) (m-p)) * (choose k p) % (choose n m)