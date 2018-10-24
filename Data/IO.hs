{-# LANGUAGE ForeignFunctionInterface #-}

module AbLib.Data.IO where

import System.Info (os)
import Data.Char (chr)

foreign import ccall "getchar" c_getchar :: IO Int

getKey :: IO Char
getKey = if os == "mingw32"
   then fmap chr c_getchar -- workaround for bug in Windows GHC
   else getChar

-- Get input until a satisfactory character is met
getUntil :: (Char -> Bool) -> IO String
getUntil f = do
   char <- getKey
   if f char
   then return []
   else fmap (char:) $ getUntil f

-- Get input of fixed length
getChars :: Int -> IO String
getChars limit = sequence $ replicate limit getKey
