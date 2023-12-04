{-# LANGUAGE LambdaCase #-}

-- should be using a lens for this

module AbLib.Data.Grid where

import qualified Data.List as List
import Data.List (intersperse, intercalate, unlines)
import Control.Applicative (liftA2)

type Coords = (Int, Int)

data Grid a = Grid Coords [[a]]
   deriving (Eq)   
   
instance Show a => Show (Grid a) where
   show g = showRows $ map (zip colWidths) rows
      where
      rows = toList (show <$> g)
      colWidths = map (maximum . map length) $ List.transpose rows
      
      topRow = genericRow ' ' ' ' '╷' ' '
      sepRow = genericRow '╶' '─' '┼' '╴'
      botRow = genericRow ' ' ' ' '╵' ' '

      genericRow :: Char -> Char -> Char -> Char -> String
      genericRow lcap mid sep rcap = let
         bulk = intercalate [sep] $ map (`replicate` mid) colWidths
         in [lcap] ++ bulk ++ [rcap]

      showRows :: [[(Int,String)]] -> String
      showRows rs = let
         bulk = intersperse sepRow $ map showRow rs
         in unlines ([topRow] ++ bulk ++ [botRow])

      showRow :: [(Int,String)] -> String
      showRow r = " " ++ (intercalate "│" $ map showCell r) ++ " "

      showCell :: (Int,String) -> String
      showCell (n,c) = pad n c

      pad :: Int -> String -> String
      pad n s = replicate (n - length s) ' ' ++ s

instance Functor Grid where
   fmap f (Grid d g) = Grid d [[f c | c <- r] | r <- g]
   
instance Foldable Grid where
   foldMap f = foldMap (foldMap f) . toList

dimensions :: Grid a -> Coords
dimensions (Grid d _) = d

width :: Grid a -> Int
width = fst . dimensions

height :: Grid a -> Int
height = snd . dimensions

toList :: Grid a -> [[a]]
toList (Grid _ g) = g

fromList :: [[a]] -> Grid a
fromList g = let
   height = length g
   widths = map length g
   width = head widths
   consistent = all (width==) widths
   in if height == 0
      then Grid (0,0) g
      else if consistent
      then Grid (width,height) g
      else errorWithoutStackTrace "Inconsistent width"

grid :: Coords -> a -> Grid a
grid (w,h) x = Grid (w,h) $ replicate h $ replicate w x

(#) :: Grid a -> Coords -> Maybe a
(#) g (x,y) = if 0 <= x && x < width g && 0 <= y && y < height g
   then Just (g #! (x,y))
   else Nothing

(#!) :: Grid a -> Coords -> a
(#!) g (x,y) = toList g !! y !! x

set :: Grid a -> Coords -> a -> Grid a
set g p c = setBy g p $ const c

setBy :: Grid a -> Coords -> (a -> a) -> Grid a
setBy g@(Grid d _) p f = Grid d $ do
   y <- [0 .. height g - 1]
   return $ do 
      x <- [0 .. width g - 1]
      [(if (x,y) == p then f else id) (g #! (x,y))]

transpose :: Grid a -> Grid a
transpose (Grid (w,h) g) = Grid (h,w) $ List.transpose g

rows :: Grid a -> [[a]]
rows = toList

cols :: Grid a -> [[a]]
cols = toList . transpose

diagonals :: Grid a -> Maybe ([a],[a])
diagonals g = let
   w = width g - 1
   fstDiag = sequence [g # (n,   n) | n <- [0..w]]
   sndDiag = sequence [g # (n, w-n) | n <- [0..w]]
   in liftA2 (,) fstDiag sndDiag

indices :: Grid a -> [Coords]
indices g = [(x,y) | x <- [0 .. width g - 1], y <- [0 .. height g - 1]]

getPos :: Eq a => a -> Grid a -> Coords
getPos x g = head $ filter (\p -> g #! p == x) $ indices g

contains :: (a -> Bool) -> Grid a -> Bool
contains f = foldr (\cell cur -> cur || f cell) False
