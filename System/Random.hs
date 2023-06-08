module AbLib.System.Random where

import System.Random

instance (Random a, Random b) => Random (a,b) where
   random g = let
      (a, g')  = random g
      (b, g'') = random g'
      in ((a, b), g'')
   randomR (lo, hi) g = let
      (a, g')  = randomR (fst lo, fst hi) g
      (b, g'') = randomR (snd lo, snd hi) g'
      in ((a, b), g'')

randomsIO :: Random a => Int -> IO [a]
randomsIO n = sequence $ replicate n randomIO

randomsRIO :: Random a => (a,a) -> Int -> IO [a]
randomsRIO range n = sequence $ replicate n $ randomRIO range

pick :: [a] -> IO a
pick xs = do
   n <- randomRIO (0, length xs - 1)
   pure (xs !! n)

pickWeighted :: (Num b, Ord b, Random b) => (a -> b) -> [a] -> IO a
pickWeighted f list = randomRIO (0, sum $ map f list) >>= selectWeighted list
   where
   selectWeighted (x:xs) p = let w = f x in if p < w
      then pure x
      else selectWeighted xs (p - w)
   selectWeighted [] _ = pickWeighted f list

shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle xs = do
   i <- randomRIO (0, length xs - 1)
   let (a,x:b) = splitAt i xs
   xs' <- shuffle (a ++ b)
   pure (x:xs')
