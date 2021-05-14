{-# LANGUAGE LambdaCase #-}

module AbLib.Data.List where

import AbLib.Data.Tuple
import Data.Maybe (isJust)
import Data.Array.IO (IOArray)
import Data.Array.MArray (readArray, writeArray, newListArray)
import System.Random (random, randomRIO)
import Control.Monad (forM)
import Data.List (transpose)

{- List of all possible splitAts -}
splits :: [a] -> [([a],[a])]
splits xs = [ splitAt n xs | n <- [0 .. length xs] ]

{- Safe element extraction -}
(!?) :: [a] -> Int -> Maybe a
xs !? n = let
   up = length $ take (n+1) xs   -- allows xs to be infinite
   in if 0 <= n && n < up        -- check valid n
      then Just (xs !! n)
      else Nothing

-- Put an item at the end of a list
append :: [a] -> a -> [a]
append xs x = xs ++ [x]

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

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\ x y -> f x == f y)

-- returns list of valid indices for a given list
indices :: [a] -> [Int]
indices xs = [0 .. length xs - 1]

-- delimit a list
delim :: Eq a => [a] -> [a] -> [[a]]
delim _ [] = [[]]
delim d (x:xs) = let
   h:t = delim d xs
   in if any (x==) d
      then [] : (h : t)
      else (x : h) : t

{- Right-pad a list with given item. -}
rpad :: Int -> a -> [a] -> [a]
rpad n x xs = take n $ xs ++ repeat x

{- Left-pad a list with given item. -}
lpad :: Int -> a -> [a] -> [a]
lpad n x xs = replicate (n - length ys) x ++ ys
   where ys = take n xs
   
data Side = L | R
pad :: Side -> Int -> a -> [a] -> [a]
pad R = rpad
pad L = lpad

alignColumns :: [Side] -> Char -> [[String]] -> [[String]]
alignColumns sides c rows = transpose $ do
   (side,col) <- zip sides $ transpose rows
   let other = \case { L -> R; R -> L }
   let maxlen = maximum $ map length col
   let col' = map (pad (other side) maxlen c) col
   return col'

-- Zips up to length of longest list, using defaults to finish the shorter list.
zipDef :: (a -> b -> c) -> (a,b) -> [a] -> [b] -> [c]
zipDef _ (_,_) []     []     = []
zipDef f (a,b) (x:xs) []     = f x b : zipDef f (a,b) xs []
zipDef f (a,b) []     (y:ys) = f a y : zipDef f (a,b) [] ys
zipDef f (a,b) (x:xs) (y:ys) = f x y : zipDef f (a,b) xs ys

-- Calculates the frequency of each item in a list.
freq :: Eq a => [a] -> [(a, Int)]
freq [] = []
freq (x:xs) = (x, 1 + count (== x) xs) : freq (filter (/= x) xs)

-- Counts the number of satisfactory items in a list.
count :: (a -> Bool) -> [a] -> Int
count f l = length $ filter f l

-- Computes the complement of a filter at the same time.
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy _ [] = ([], [])
splitBy f (x:xs) = if f x
   then (x:ys, ns)
   else (ys, x:ns)
   where (ys, ns) = splitBy f xs

-- Picks a number of random items from a list, without repetition.
sample :: Int -> [a] -> IO [a]
sample k xs = do
   ar <- newArray n xs
   forM [1..k] $ \i -> do
      j <- randomRIO (i,n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
   where
      n = length xs
      newArray :: Int -> [a] -> IO (IOArray Int a)
      newArray n xs = newListArray (1,n) xs

-- Shuffle a list.
shuffle :: [a] -> IO [a]
shuffle xs = sample (length xs) xs

-- Uses binary search to determine if a list contains a given item.
binSearch :: Ord a => a -> [a] -> Bool
binSearch t xs = isJust $ binSearchStrong t xs

-- Uses exponential search to determine if a list contains a given item.
expSearch :: Ord a => a -> [a] -> Bool
expSearch t xs = isJust $ expSearchStrong t xs

-- Uses binary search to find largest index i such that (xs !! i <= t).
binSearchWeak :: (Ord a) => a -> [a] -> Maybe Int
binSearchWeak _ [] = Nothing
binSearchWeak t [m] = if t < m then Nothing else Just 0
binSearchWeak t xs = let
   n = div (length xs) 2
   (a, b) = splitAt n xs
   in if t < head b
      then binSearchWeak t a
      else fmap (+ length a) (binSearchWeak t b)

-- Uses exponential search to find largest index i such that (xs !! i <= t)
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

-- Uses binary search to find the index of a given item in a list.
binSearchStrong :: (Ord a) => a -> [a] -> Maybe Int
binSearchStrong t xs = do
   i <- binSearchWeak t xs
   if xs !! i == t
   then Just i
   else Nothing

-- Uses exponential search to find the index of a given item in a list.
expSearchStrong :: (Ord a) => a -> [a] -> Maybe Int
expSearchStrong t xs = do
   i <- expSearchWeak t xs
   if xs !! i == t
   then Just i
   else Nothing
