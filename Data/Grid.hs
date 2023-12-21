{-# LANGUAGE LambdaCase #-}

-- should be using a lens for this

module AbLib.Data.Grid where

import qualified Data.List as List
import Data.List (intersperse, intercalate, unlines)
import Control.Applicative (liftA2)
import Data.Maybe (catMaybes)

type Coords = (Int, Int)

data Grid a = Grid GridInfo [[a]]
   deriving (Eq)

data GridInfo = GridInfo
   { dimensions' :: Coords
   , gridLines' :: Bool
   } deriving (Show, Eq)

disableGridLines :: Grid a -> Grid a
disableGridLines (Grid (GridInfo c _) g) = Grid (GridInfo c False) g

gridLines :: Grid a -> Bool
gridLines (Grid i _) = gridLines' i
   
instance Show a => Show (Grid a) where
   show g = showRows $ map (zip colWidths) rows
      where
      rows = toList (show <$> g)
      colWidths = map (maximum . map length) $ List.transpose rows
      cellSep = if gridLines g then "|" else ""
      padding = if gridLines g then " " else ""
      
      topRow = genericRow ' ' ' ' '╷' ' '
      sepRow = genericRow '╶' '─' '┼' '╴'
      botRow = genericRow ' ' ' ' '╵' ' '

      genericRow :: Char -> Char -> Char -> Char -> String
      genericRow lcap mid sep rcap = let
         bulk = intercalate [sep] $ map (`replicate` mid) colWidths
         in [lcap] ++ bulk ++ [rcap]

      showRows :: [[(Int,String)]] -> String
      showRows rs = let
         content = map showRow rs
         bulk = intersperse sepRow content
         in if gridLines g
            then unlines ([topRow] ++ bulk ++ [botRow])
            else unlines content

      showRow :: [(Int,String)] -> String
      showRow r = padding ++ (intercalate cellSep $ map showCell r) ++ padding

      showCell :: (Int,String) -> String
      showCell (n,c) = pad n c

      pad :: Int -> String -> String
      pad n s = replicate (n - length s) ' ' ++ s

instance Functor Grid where
   fmap f (Grid i g) = Grid i [[f c | c <- r] | r <- g]
   
instance Foldable Grid where
   foldMap f = foldMap (foldMap f) . toList

dimensions :: Grid a -> Coords
dimensions (Grid i _) = dimensions' i

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
      then Grid (GridInfo (0,0) True) g
      else if consistent
      then Grid (GridInfo (width,height) True) g
      else errorWithoutStackTrace "Inconsistent width"

grid :: Coords -> a -> Grid a
grid (w,h) x = Grid (GridInfo (w,h) True) $ replicate h $ replicate w x

(#) :: Grid a -> Coords -> Maybe a
(#) g (x,y) = if 0 <= x && x < width g && 0 <= y && y < height g
   then Just (g #! (x,y))
   else Nothing

(#!) :: Grid a -> Coords -> a
(#!) g (x,y) = toList g !! y !! x

set :: Grid a -> Coords -> a -> Grid a
set g p c = setBy g p $ const c

setBy :: Grid a -> Coords -> (a -> a) -> Grid a
setBy g@(Grid i _) p f = Grid i $ do
   y <- [0 .. height g - 1]
   return $ do 
      x <- [0 .. width g - 1]
      [(if (x,y) == p then f else id) (g #! (x,y))]

transpose :: Grid a -> Grid a
transpose (Grid i g) = Grid (aux i) $ List.transpose g
   where
   aux (GridInfo (w,h) l) = GridInfo (h,w) l

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

findAll :: (a -> Bool) -> Grid a -> [Coords]
findAll f g = filter (\p -> f (g #! p)) $ indices g

neighbourhood :: Coords -> Grid a -> [a]
neighbourhood (x,y) g = catMaybes [g # (x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1]]
