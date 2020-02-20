{-# LANGUAGE LambdaCase #-}

module AbLib.Data.Stack (
   Stack, fromList, toList, empty,
   modifyStack, push, pop, peek, pushMaybe
) where

import Data.IORef
import System.IO.Unsafe
import Data.Maybe (listToMaybe)
import Control.Monad

newtype Stack a = Stack {toIORef :: IORef [a]}

instance Show a => Show (Stack a) where
   show = show . unsafePerformIO . toList

fromList :: [a] -> IO (Stack a)
fromList xs = newIORef xs >>= return . Stack

toList :: Stack a -> IO [a]
toList = readIORef . toIORef

-- Requires explicit type declaration on use.
empty :: IO (Stack a)
empty = fromList []

modifyStack :: ([a] -> [a]) -> Stack a -> IO ()
modifyStack f = flip modifyIORef f . toIORef

push :: a -> Stack a -> IO ()
push x = modifyStack (x:)

pushMaybe :: Maybe a -> Stack a -> IO ()
pushMaybe x = case x of
   Nothing -> const $ return ()
   Just y  -> push y

pop :: Stack a -> IO (Maybe a)
pop s = do
   x <- peek s
   unless (null x) $ modifyStack tail s
   return x
   
peek :: Stack a -> IO (Maybe a)
peek s = do
   xs <- toList s
   return $ listToMaybe xs
