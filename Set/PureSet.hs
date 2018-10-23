{-# LANGUAGE MultiParamTypeClasses #-}

module AbLib.Set.PureSet (PureSet) where

import AbLib.Sets.Set
import Data.List (nub)

newtype PureSet = PureSet [PureSet]

removeOne :: Eq a => a -> [a] -> Maybe [a]
removeOne _ []     = Nothing
removeOne t (x:xs) = if x == t
   then Just xs
   else fmap (x:) $ removeOne t xs

instance Show PureSet where
   show (PureSet xs) = "{" ++ (init . tail . show) xs ++ "}"

instance Eq PureSet where
   PureSet [] == PureSet [] = True
   PureSet xs == PureSet ys = all (`elem` ys) xs && all (`elem` xs) ys

instance Set PureSet PureSet where
   empty = PureSet []
   singleton e = PureSet [e]
   fromList xs = PureSet $ nub xs
   element x = head $ elements x

   isEmpty x = null $ elements x
   contains x e = elem e $ elements x
   
   subset x y = subset' (elements x) (elements y) where
      subset' [] _     = True                      -- empty is always a subset
      subset' _  []    = False                     -- nothing is a subset of empty (except empty)
      subset' (e:s) t  = case removeOne e t of     -- for each element e of set x
         Nothing -> False                          -- e not in y => x is not subset of y
         Just t' -> subset' s t'                   -- e in y => continue checking

   distinct _ y | isEmpty y = True                 -- empty shares no elements with any set
   distinct x y = distinct' $ elements x where
      distinct' []    = True                       -- empty shares no elements with any set
      distinct' (e:s) = if y `contains` e
         then False                                -- x and y share a common element e
         else distinct' s                          -- continue searching for a common element

   add x e | x `contains` e = x
   add x e = PureSet (e : elements x)

   remove x e = case removeOne e $ elements x of
      Nothing -> x
      Just x' -> PureSet x'

   union x y | size x < size y = union y x
   union x y = foldl add x $ elements y

   intersect x y | size x < size y = intersect y x
   intersect x y = PureSet $ intersect' (elements x) (elements y) where
      intersect' []    _ = []
      intersect' (e:s) t = case removeOne e t of   -- for each element e in set x
         Nothing -> intersect' s t                 -- e not in set y => don't include e in intersection
         Just t' -> e : intersect' s t'            -- e in set y     => include e in intersection

   complement x y = PureSet $ complement' (elements x) (elements y) where
      complement' []    _ = []
      complement' (e:s) t = case removeOne e t of  -- for each element e in set x
         Nothing -> e : complement' s t            -- e not in set y => include e in complement
         Just t' -> complement' s t'               -- e in set y => don't include e in complement
   
   sup x | isEmpty x = Nothing
   sup x = return $ maximum $ elements x
   inf x | isEmpty x = Nothing
   inf x = return $ minimum $ elements x

instance DiscreteSet PureSet PureSet where
   elements (PureSet xs) = xs
   smap f = fromList . map f . elements
   sfilter f = PureSet . filter f . elements
