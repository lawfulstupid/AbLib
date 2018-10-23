{-# LANGUAGE ScopedTypeVariables #-}

module AbLib.Util.Safe where

import Control.Exception (SomeException, try)
import GHC.IO (evaluate, unsafePerformIO)

-- Returns Nothing if the supplied value throws an Exception.
pacify :: a -> Maybe a
pacify x = unsafePerformIO $ do
   e <- try $ evaluate x
   return $ case e of
      Left (_ :: SomeException) -> Nothing
      Right _ -> Just x

safe0 = (pacify)
safe1 = (safe0.) ; safe = safe1
safe2 = (safe1.)
safe3 = (safe2.)
safe4 = (safe3.)
-- should probably do
