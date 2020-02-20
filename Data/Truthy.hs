
module AbLib.Data.Truthy where

import Data.Ratio
import Data.Void
import Data.Maybe
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.Map as Map; import Data.Map (Map)

class Truthy a where
   truthy :: a -> Bool
   
instance Truthy Void where
   truthy = const False

instance Truthy () where
   truthy () = True

instance Truthy Bool where
   truthy = id

instance Truthy Int where
   truthy = (0 /=)

instance Truthy Word where
   truthy = (0 /=)

instance Truthy Integer where
   truthy = (0 /=)

instance Truthy Float where
   truthy = (0 /=)

instance Truthy Double where
   truthy = (0 /=)

instance Truthy a => Truthy (Ratio a) where
   truthy = truthy . numerator

instance Truthy Char where
   truthy = ('\0' ==)



instance Truthy [a] where
   truthy = not . null
   
instance (Truthy a, Truthy b) => Truthy (a,b) where
   truthy (a,b) = truthy a && truthy b

instance (Truthy a, Truthy b, Truthy c) => Truthy (a,b,c) where
   truthy (a,b,c) = truthy (a,(b,c))

instance (Truthy a, Truthy b, Truthy c, Truthy d) => Truthy (a,b,c,d) where
   truthy (a,b,c,d) = truthy (a,(b,c,d))

instance (Truthy a, Truthy b, Truthy c, Truthy d, Truthy e) => Truthy (a,b,c,d,e) where
   truthy (a,b,c,d,e) = truthy (a,(b,c,d,e))

instance (Truthy a, Truthy b, Truthy c, Truthy d, Truthy e, Truthy f) => Truthy (a,b,c,d,e,f) where
   truthy (a,b,c,d,e,f) = truthy (a,(b,c,d,e,f))

instance (Truthy a, Truthy b, Truthy c, Truthy d, Truthy e, Truthy f, Truthy g) => Truthy (a,b,c,d,e,f,g) where
   truthy (a,b,c,d,e,f,g) = truthy (a,(b,c,d,e,f,g))

instance (Truthy a, Truthy b, Truthy c, Truthy d, Truthy e, Truthy f, Truthy g, Truthy h) => Truthy (a,b,c,d,e,f,g,h) where
   truthy (a,b,c,d,e,f,g,h) = truthy (a,(b,c,d,e,f,g,h))

instance (Truthy a, Truthy b, Truthy c, Truthy d, Truthy e, Truthy f, Truthy g, Truthy h, Truthy i) => Truthy (a,b,c,d,e,f,g,h,i) where
   truthy (a,b,c,d,e,f,g,h,i) = truthy (a,(b,c,d,e,f,g,h,i))

instance Truthy (Maybe a) where
   truthy = isJust

instance Truthy (Set a) where
   truthy = not . Set.null

instance Truthy (Map a) where
   truthy = not . Map.null

