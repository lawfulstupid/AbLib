{-# LANGUAGE LambdaCase, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

import AbLib.Sets.IsSet
import Data.List (nub)

newtype SetBase a = SetBase [ElmType a]

data Element a
data PURESET

type family ElmType a
type instance ElmType (Element a) = a
type instance ElmType (PURESET) = PureSet

type Set a   = SetBase (Element a)
type PureSet = SetBase (PURESET)

deriving instance Eq (ElmType a) => Eq (SetBase a)
deriving instance Ord (ElmType a) => Ord (SetBase a)
-- deriving instance Show (ElmType a) => Show (SetBase a)
instance Show (ElmType a) => Show (SetBase a) where
   show (SetBase xs) = "{" ++ (init . tail . show) xs ++ "}"
   
getelms (SetBase x) = x
   
-- instance Foldable SetBase where
--    foldr :: (a -> b -> b) -> b -> SetBase a -> b
--    foldr f z s = foldr f z $ elements s

-- instance IsSet (SetBase a) (ElmType a) where
--    empty = SetBase []
--    singleton e = SetBase [e]
--    fromList = SetBase . nub
--    elements (SetBase xs) = xs
-- 
--    add x e | x `contains` e = x
--    add x e = SetBase (e : elements x)
-- 
--    remove x e = SetBase $ remove' $ elements x where
--       remove' [] = []
--       remove' (y:ys) | y == e = ys
--       remove' (y:ys) = y : remove' ys
-- 
--    union x y | size x < size y = union y x
--    union x y = foldl add x $ elements y
-- 
--    intersect x y | size x < size y = intersect y x
--    intersect x y = SetBase $ aux y $ elements x where
--       aux _   []       = []
--       aux tst (e:elms) = let
--          tst' = remove tst e  -- reduce the size of the test set where possible
--          op = if size tst' == size tst then id else (e:)
--          in op $ aux tst' elms
 























