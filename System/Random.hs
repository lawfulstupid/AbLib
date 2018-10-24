

module AbLib.System.Random (randomsIO, randomsRIO, randomIO, randomRIO) where

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
