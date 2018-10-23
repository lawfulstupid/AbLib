fib :: Int -> Integer
fib = (fiblist!!)

-- list of all fibonacci numbers
fiblist :: [Integer]
fiblist = map def [0..] where
    def :: Int -> Integer
    def 0 = 0
    def 1 = 1
    def n = fib (n-1) + fib (n-2)

-- limits fiblist to values < n
limit :: Integer -> [Integer]
limit n = aux fiblist where
    aux :: [Integer] -> [Integer]
    aux (f:fs)
        | f < n = f : (aux fs)
        | otherwise = []

-- returns index of first fibonacci number >= n
nextFib :: Integer -> Int
nextFib t = auxe (0, 1) where
    auxe :: (Int, Int) -> Int
    auxe (a, b) = if t < fib b
                  then a + (auxb $ map fib [a..b])
                  else auxe (b, b*2)
    auxb :: [Integer] -> Int
    auxb [] = 0
    auxb [a] = 1
    auxb l = let n = quot (length l) 2 in
             if t <= l !! n
             then auxb (take n l)
             else n + auxb (drop n l)

-- returns the index of the search target in the list
-- assumes list is sorted
expSearch :: Ord a => a -> [a] -> Maybe Int
expSearch t [] = Nothing
expSearch t [a] = if t == a then Just 0 else Nothing
expSearch t l = aux (0, 1) where
    aux :: (Int, Int) -> Maybe Int
    aux (a, b) = if t < l !! b
                 then sumMaybe [Just a, binSearch t $ map (l !!) [a..b]]
                 else aux (b, b*2)

-- returns the index of the search target is the list
-- assumes the list is sorted and finite
binSearch :: Ord a => a -> [a] -> Maybe Int
binSearch t [] = Nothing
binSearch t [a] = if t == a then Just 0 else Nothing
binSearch t l = let n = quot (length l) 2 in
                if t < l !! n
                then binSearch t (take n l)
                else sumMaybe [Just n, binSearch t $ drop n l]
                 
sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = fmap sum . sequence