{-# LANGUAGE LambdaCase #-}

module AbLib.Data.Stack (
   Stack, fromList, toList, newStack,
   push, pop, peek, modifyStack
) where

import Data.IORef
import System.IO.Unsafe
import Data.Maybe (listToMaybe)
import Control.Monad

data Stack a = Stack {toIORef :: IORef [a]}

instance Show a => Show (Stack a) where
   show = unsafeShowStack

unsafeShowStack :: Show a => Stack a -> String
unsafeShowStack = show . unsafePerformIO . toList

fromList :: [a] -> IO (Stack a)
fromList xs = newIORef xs >>= return . Stack

toList :: Stack a -> IO [a]
toList = readIORef . toIORef

newStack :: IO (Stack a)
newStack = fromList []

modifyStack :: ([a] -> [a]) -> Stack a -> IO ()
modifyStack f = flip modifyIORef f . toIORef

push :: a -> Stack a -> IO ()
push x = modifyStack (x:)

pop :: Stack a -> IO (Maybe a)
pop s = do
   x <- peek s
   unless (null x) $ modifyStack tail s
   return x
   
peek :: Stack a -> IO (Maybe a)
peek s = do
   xs <- toList s
   return $ listToMaybe xs
