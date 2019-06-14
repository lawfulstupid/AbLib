{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module AbLib.Math.Matrix (
   Matrix, printMatrix, fromList, fromList', toList,
   rc, rows, cols, zeroSize, det,
   (!), getRow, getCol, setRow, setCol,
   rowSwap, rowScale, rowAddBy, rowAdd, rowSub,
   minor, pivot, echelon,
   zero, ident, coIdent
) where

import AbLib.Data.List ((!?), del, set, lpad)
import Data.List (nub)
import Data.Maybe (fromJust)

data Matrix a = Matrix [[a]]
   deriving (Eq)

toList :: Matrix a -> [[a]]
toList (Matrix m) = m

fromList :: [[a]] -> Matrix a
fromList m = case nub $ map length m of
   [ ] -> error "empty matrix!"           -- no rows
   [0] -> error "empty matrix!"           -- rows are zero-length
   [_] -> Matrix m                        -- unique nonzero row length
   ___ -> error "unequal row lengths!"    -- multiple row lengths

{- Create given width and value list in right-to-left top-to-bottom order. -}
fromList' :: Int -> [a] -> Matrix a
fromList' n xs = fromList $ aux xs
   where
   aux :: [a] -> [[a]]
   aux [] = []
   aux xs = a : aux b where (a,b) = splitAt n xs

instance Show a => Show (Matrix a) where
   show = show . toList
   
printMatrix :: Show a => Matrix a -> IO ()
printMatrix (Matrix m) = do
   let itemLen = maximum $ map (length . show) $ concat m
   let pad = lpad itemLen ' '
   flip mapM_ m $ \ r -> do 
      flip mapM_ r $ \ x -> do
         putStr $ pad $ show x
         putStr " "
      putStr "\n"

instance Functor Matrix where
   fmap f (Matrix m) = Matrix $ map (map f) m
   
instance Num a => Num (Matrix a) where
   a + b = if rc a /= rc b
      then error "mismatching dimensions!"
      else Matrix $ zipWith (zipWith (+)) (toList a) (toList b)
   
   -- Dot product of matrices
   a * b = let
      (ra,ca) = rc a
      (rb,cb) = rc b
      f i j = sum [(a ! (i,k)) * (b ! (k,j)) | k <- [0..ca-1]]
      in if ca /= rb
         then error "mismatching dimensions!"
         else Matrix [[f i j | j <- [0..cb-1]] | i <- [0..ra-1]]
   
   negate      = fmap negate
   fromInteger = fromList' 1 . return . fromInteger

rc :: Matrix a -> (Int, Int)
rc m = (rows m, cols m)

rows :: Matrix a -> Int
rows (Matrix m) = length m

cols :: Matrix a -> Int
cols (Matrix m) = length (m !! 0)

zeroSize :: Matrix a -> Bool
zeroSize m = rows m == 0 || cols m == 0

(!) :: Matrix a -> (Int, Int) -> a
(!) (Matrix m) (i,j) = m !! i !! j

getRow :: Int -> Matrix a -> [a]
getRow i (Matrix m) = case m !? i of
   Nothing -> []
   Just rw -> rw

setRow :: Int -> [a] -> Matrix a -> Matrix a
setRow i row (Matrix m) = if length row == cols (Matrix m)
   then Matrix $ set i m row
   else error "mismatching row length!"

getCol :: Int -> Matrix a -> [a]
getCol i (Matrix m) = case map (!? i) m of
   Nothing : _ -> []
   justColumn  -> map fromJust justColumn

setCol :: Int -> [a] -> Matrix a -> Matrix a
setCol i col (Matrix m) = if length col == rows (Matrix m)
   then Matrix $ zipWith (set i) m col
   else error "mismatching column length!"

-- Compute the determinant of a matrix
det :: Num a => Matrix a -> a
det m | zeroSize m    = error "only square matrices have determinants!"
det m | rc m == (1,1) = m ! (0,0)
det m = let
   parity i = if mod i 2 == 0 then 1 else -1
   term i = parity i * m ! (0,i) * det (minor m (0,i))
   in sum $ map term [0 .. cols m - 1]

rowScale :: (Num a) => Matrix a -> Int -> a -> Matrix a
rowScale m i sf = setRow i (map (sf *) $ getRow i m) m

rowSwap :: Matrix a -> (Int, Int) -> Matrix a
rowSwap m (i,j) = let
   row_i = getRow i m
   row_j = getRow j m
   m' = setRow i row_j m
   in setRow j row_i m'

rowAdd :: (Num a) => Int -> Int -> Matrix a -> Matrix a
rowAdd = rowAddBy 1

rowSub :: (Num a) => Int -> Int -> Matrix a -> Matrix a
rowSub = rowAddBy (-1)

rowAddBy :: (Num a) => a -> Int -> Int -> Matrix a -> Matrix a
rowAddBy sf src tgt m = let
   srcRow = getRow src m
   tgtRow = getRow tgt m
   in setRow tgt (zipWith (\ x y -> sf * x + y) srcRow tgtRow) m

{- Sets the index to 1 and all other values in the column to 0 using row operations. -}
pivot :: Fractional a => (Int,Int) -> Matrix a -> Matrix a
pivot (i,j) m = let
   p  = m ! (i,j)
   m' = rowScale m i $ recip p
   domain = [0 .. i-1] ++ [i+1 .. rows m - 1]
   subRow r = rowAddBy (negate $ m ! (r,j)) i r
   in foldr subRow m' domain

-- Extracts a minor or a matrix
minor :: Matrix a -> (Int, Int) -> Matrix a
minor (Matrix m) (i,j) = Matrix $ map (del j) $ del i m

-- Transform a matrix into reduced upper echelon form
echelon :: (Fractional a) => Matrix a -> Matrix a
echelon m | zeroSize m = m
echelon m = let
   maxrow = rows m - 1
   m1 = rowScale m 0 $ recip (m ! (0,0))     -- scale the first row to have a leading 1
   m0 = zeroLead maxrow m1                   -- add first row to other rows to have leading 0s
   mr = toList $ echelon $ minor m0 (0,0)    -- recurse on minor
   mt = Matrix (getRow 0 m0 : map (0:) mr)   -- reconstruct m with minor in reduce echelon form
   in zeroCoeffs maxrow mt                   -- make the nonleading coefficients of the first row zero
   where
   zeroLead :: Num a => Int -> Matrix a -> Matrix a
   zeroLead 0 m = m
   zeroLead i m = let
      sf = m ! (i,0)             -- leading term of row i
      m' = rowAddBy (-sf) 0 i m  -- row i reduced by row 0 to have leading 0
      in zeroLead (i-1) m'       -- repeat on other rows
   zeroCoeffs :: Num a => Int -> Matrix a -> Matrix a
   zeroCoeffs 0 m = m
   zeroCoeffs i m = let
      sf = m ! (0,i)
      m' = rowAddBy (-sf) i 0 m
      in zeroCoeffs (i-1) m'

-- Zero everywhere
zero :: Num a => (Int, Int) -> Matrix a
zero (r,c) = Matrix $ replicate r $ replicate c 0

-- Identity matrix: 1s along the main diagonal and 0 elsewhere
ident :: Num a => Int -> Matrix a
ident n = Matrix [[if x==y then 1 else 0 | x <- [1..n]] | y <- [1..n]]

-- Not sure if this has a name; it's a flipped identity matrix
coIdent :: Num a => Int -> Matrix a
coIdent n = let
   row i = replicate (n-1-i) 0 ++ [1] ++ replicate i 0
   in Matrix $ map row [0..n-1]
