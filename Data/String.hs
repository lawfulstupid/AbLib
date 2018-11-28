-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

module AbLib.Data.String where

-- For types that exhibit behaviour similar to String
class StringLike s where
   toString :: s -> String
   
-- this actually works really nicely
instance StringLike s => StringLike [s] where
   toString = foldr (++) "" . map toString
   
instance StringLike Char where
   toString = return

