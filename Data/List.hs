module AbLib.Data.List where

-- | Safe element extraction
(!?) :: [a] -> Int -> Maybe a
xs !? n = let
   up = length $ take (n+1) xs   -- allows xs to be infinite
   in if 0 <= n && n < up        -- check valid n
      then Just (xs !! n)
      else Nothing

-- | Delete element at index
del :: Int -> [a] -> [a]
del i xs = case splitAt i xs of
   (h, _ : t) -> h ++ t
   __________ -> xs

-- | Sets the ith element if possible
set :: Int -> [a] -> a -> [a]
set i xs x = case splitAt i xs of
   (h, _ : t) -> h ++ [x] ++ t
   __________ -> xs

setAll :: [Int] -> [a] -> a -> [a]
setAll [    ] xs _ = xs
setAll (i:is) xs x = setAll is (set i xs x) x

-- | Equal to `elem t xs`
binSearch :: Ord a => a -> [a] -> Bool
binSearch t xs = Nothing /= binSearchStrong t xs

-- | Equal to `elem t xs`
expSearch :: Ord a => a -> [a] -> Bool
expSearch t xs = Nothing /= expSearchStrong t xs

-- | Returns largest `i` such that `xs !! i <= t`
binSearchWeak :: (Ord a) => a -> [a] -> Maybe Int
binSearchWeak _ [] = Nothing
binSearchWeak t [m] = if t < m then Nothing else Just 0
binSearchWeak t xs = let
   n = div (length xs) 2
   (a, b) = splitAt n xs
   in if t < head b
      then binSearchWeak t a
      else fmap (+ length a) (binSearchWeak t b)

-- | Returns largest `i` such that `xs !! i <= t`
expSearchWeak :: (Ord a) => a -> [a] -> Maybe Int
expSearchWeak t xs = fmap (+ d) $ binSearchWeak t sub
   where
   (d, sub) = aux 1 xs
   aux _ [] = (0, [])
   aux n xs = let
      (a, b) = splitAt n xs
      (k, l) = aux (2*n) b
      in if t < head b
         then (0, a)
         else (k + length a, l)

-- | Returns `i` such that `xs !! i == t`
binSearchStrong :: (Ord a) => a -> [a] -> Maybe Int
binSearchStrong t xs = do
   i <- binSearchWeak t xs
   if xs !! i == t
   then Just i
   else Nothing

-- | Return `i` such that `xs !! i == t`
expSearchStrong :: (Ord a) => a -> [a] -> Maybe Int
expSearchStrong t xs = do
   i <- expSearchWeak t xs
   if xs !! i == t
   then Just i
   else Nothing

