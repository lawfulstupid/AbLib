module AbLib.Util.Utilities where

import System.Random (randomRIO)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (NFData, deepseq)
import Data.Array.IO
import Control.Monad (forM)
import Data.List (intercalate)

eval2 :: (a -> b, a -> c) -> a -> (b, c)
eval2 (f,g) x = (f x, g x)

eval3 :: (a -> b, a -> c, a -> d) -> a -> (b, c, d)
eval3 (f,g,h) x = (f x, g x, h x)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x,y) = (f x, f y)
   
mean :: (Real a, Fractional b) => [a] -> b
mean ns = (toFrac $ sum ns) / (toFrac $ length ns)

toFrac :: (Real a, Fractional b) => a -> b
toFrac = fromRational . toRational

printLn :: Show a => [[a]] -> IO ()
printLn xs = putStrLn $ intercalate "\n" $ map (intercalate " " . map show) xs

