{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module AbLib.Data.Counter where

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

data Counter a = Dummy Integer | Count Integer a
   deriving (Eq, Ord)

count :: Counter a -> Integer
count (Dummy n) = n
count (Count n _) = n

count' :: (a -> Counter a) -> Integer
count' = count . ($undefined)

mapCount :: (Integer -> Integer) -> Counter a -> Counter a
mapCount f (Dummy n) = Dummy (f n)
mapCount f (Count n x) = Count (f n) x

item :: Counter a -> Maybe a
item (Dummy _) = Nothing
item (Count _ x) = Just x

instance Show a => Show (Counter a) where
   show (Dummy n) = show n
   show (Count n x) = show n ++ " " ++ show x

instance Eq a => Num (Counter a) where
   fromInteger = Dummy
   
   Dummy n + Dummy m   = Dummy (n+m)
   Dummy n + Count m x = Count (n+m) x
   Count n x + Dummy m = Count (n+m) x
   Count n x + Count m y | x == y = Count (n+m) x
   
   Dummy n * Dummy m   = Dummy (n*m)
   Dummy n * Count m x = Count (n*m) x
   Count n x * Dummy m = Count (n*m) x
   Count n x * Count m y | x == y = Count (n*m) x
   
   negate = mapCount negate
   signum = mapCount signum
   abs    = mapCount abs

instance Num (a -> Counter a) where
   fromInteger = Count
   
   f + g = Count (count' f + count' g)
   f * g = Count (count' f * count' g)
   
   negate = Count . negate . count'
   signum = Count . signum . count'
   abs    = Count . abs    . count'
   

data Stock a = Stock Integer (Map a Integer)
   deriving (Eq)

instance Show a => Show (Stock a) where
   show (Stock c m) = let
      h = Dummy c
      t = map (uncurry $ flip Count) $ Map.toList m
      in "{" ++ (intercalate ", " $ map show (h:t)) ++ "}"

instance Eq a => Num (Stock a) where
   fromInteger n = Stock n Map.empty
   
   Stock n xs + Stock m ys = Stock (n+m) $ merge xs ys

merge :: Eq a => Map a Integer -> Map a Integer -> Map a Integer
merge xs ys
   | size xs > size ys = merge ys xs
   | null xs = ys
   | otherwise = let
      (x',xs') = Map.splitAt 1 xs
      x = head $ Map.assocs x
