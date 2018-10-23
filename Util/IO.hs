{-# LANGUAGE ForeignFunctionInterface #-}

module Util.IO where

-- Get input until a specific character is pressed
getUntil :: Char -> IO String
getUntil term = do
   char <- getChar
   if char == term
   then return []
   else fmap (char:) $ getUntil term

-- Get input of fixed length
getChars :: Int -> IO String
getChars limit = sequence $ replicate limit getChar
