{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AbLib.Data.String where

class ToString a where
   toString :: a -> String

instance {-# OVERLAPPING #-} ToString Char where
   toString = (:[])
   
instance {-# OVERLAPPING #-} ToString String where
   toString = id

instance Show a => ToString a where
   toString = show