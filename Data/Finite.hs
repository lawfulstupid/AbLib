{-# LANGUAGE LambdaCase, EmptyCase #-}


module AbLib.Data.Finite where

import Data.Maybe (fromJust)
import Data.List
import GHC.Real (Ratio (..))
import Data.Void
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.ST (ST(..))

class Finite a where
   domain :: [a]
   
   
instance Finite () where
   domain = [()]
   
instance Finite Int where
   domain = [minBound ..]

instance Finite Word where
   domain = [minBound ..]
   
instance Finite Char where
   domain = [minBound ..]

instance Finite Bool where
   domain = [minBound ..]

instance Finite Ordering where
   domain = [minBound ..]
   
instance Finite Void where
   domain = []
   

instance (Finite a, Finite b) => Finite (a,b) where
   domain = [(a,b) | a <- domain, b <- domain]
   
instance (Finite a, Finite b, Finite c) => Finite (a,b,c) where
   domain = [(a,b,c) | (a,(b,c)) <- domain]

instance (Finite a, Finite b, Finite c, Finite d) => Finite (a,b,c,d) where
   domain = [(a,b,c,d) | (a,(b,c,d)) <- domain]
   
instance (Finite a) => Finite (Maybe a) where
   domain = Nothing : map Just domain
   
instance (Finite a, Finite b) => Finite (Either a b) where
   domain = map Left domain ++ map Right domain
   
instance (Finite a) => Finite (Ratio a) where
   domain = uncurry (:%) <$> domain
   
instance (Finite a) => Finite (Set a) where
   domain = Set.fromDistinctAscList <$> subsequences domain
   
instance (Finite a, Finite b) => Finite (Map a b) where
   domain = let
      xs  = domain -- [a]
      yss = sequence $ replicate (length xs) domain -- [[b]]
      in Map.fromDistinctAscList . zip xs <$> yss

-- very slow
instance (Eq a, Finite a, Finite b) => Finite (a -> b) where
   domain = let
      xs  = domain
      yss = sequence $ replicate (length xs) domain
      in map ((fromJust .) . flip lookup . zip xs) yss

instance (Finite a) => Finite (IO a) where
   domain = return <$> domain

instance (Finite s, Finite a) => Finite (ST s a) where
   domain = return <$> domain