
module AbLib.Data.StackRef (
   Stack, fromList, toList,
   enreference, dereference,
   push, pop, peek
) where

import qualified AbLib.Data.Stack as R
import Data.IORef
import System.IO.Unsafe
import Data.Maybe (listToMaybe)
import Control.Monad

--------------------------------------------------------------------------------

newtype Stack a = Stack (IORef (R.Stack a))

enreference :: R.Stack a -> IO (Stack a)
enreference = fmap Stack . newIORef

modifyStack :: Stack a -> (R.Stack a -> R.Stack a) -> IO ()
modifyStack (Stack r) f = modifyIORef' r f

dereference :: Stack a -> IO (R.Stack a)
dereference (Stack r) = readIORef r

--------------------------------------------------------------------------------

instance Show a => Show (Stack a) where
   show s = show $ unsafePerformIO $ dereference s

instance Semigroup (Stack a) where
   Stack a <> Stack b = unsafePerformIO $ do
      a' <- readIORef a
      b' <- readIORef b
      enreference (a' <> b')

instance Monoid (Stack a) where
   mempty = unsafePerformIO $ enreference mempty

--------------------------------------------------------------------------------

fromList :: [a] -> IO (Stack a)
fromList = enreference . R.fromList

toList :: Stack a -> IO [a]
toList = fmap R.toList . dereference

push :: a -> Stack a -> IO ()
push x s = modifyStack s $ R.push x

pop :: Stack a -> IO (Maybe a)
pop s = do
   (x, xs) <- R.pop <$> dereference s
   modifyStack s $ const xs
   return x
   
peek :: Stack a -> IO (Maybe a)
peek s = R.peek <$> dereference s
