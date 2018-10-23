{-# LANGUAGE ForeignFunctionInterface #-}

module Util.IO where

import System.Info (os)
import Data.Char (chr)
import Foreign.C.Types (CInt (..))
foreign import ccall unsafe "conio.h getch"
   c_getch :: IO CInt

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

getKey :: IO Char
getKey = if os == "mingw32"            -- check if OS is Windows
   then fmap (chr . fromEnum) c_getch  -- workaround for bug in Windows GHC
   else getChar                        -- works on UNIX