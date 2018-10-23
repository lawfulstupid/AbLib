module Math.Matrix (
   Matrix, fromList, toList,
   rc, rows, cols, zeroSize, det,
   (!), getRow, getCol, setRow, setCol, minor,
   echelon, rowSwap, rowScale, rowAddBy, rowAdd, rowSub,
   dot,
   zero, ident, coIdent
) where

import Data.List (nub)
import Util.List ((!?), del, set)
import Data.Maybe (fromJust)

data Matrix a = Matrix { toList :: [[a]] } deriving (Eq)

fromList :: [[a]] -> Matrix a
fromList m = case nub $ map length m of
   [ ] -> error "empty matrix!"           -- no rows
   [0] -> error "empty matrix!"           -- rows are zero-length
   [_] -> Matrix m                        -- unique nonzero row length
   ___ -> error "unequal row lengths!"    -- multiple row lengths

instance Show a => Show (Matrix a) where
   show = show . toList

instance Functor Matrix where
   fmap f (Matrix m) = Matrix $ map (map f) m

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

setRow :: Int -> Matrix a -> [a] -> Matrix a
setRow i (Matrix m) row = if length row == cols (Matrix m)
   then Matrix $ set i m row
   else error "mismatching row length!"

getCol :: Int -> Matrix a -> [a]
getCol i (Matrix m) = case map (!? i) m of
   Nothing : _ -> []
   justColumn  -> map fromJust justColumn

setCol :: Int -> Matrix a -> [a] -> Matrix a
setCol i (Matrix m) col = if length col == rows (Matrix m)
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

-- Dot product of matrices
dot :: Num a => Matrix a -> Matrix a -> Matrix a
dot a b = let
   (ra,ca) = rc a
   (rb,cb) = rc b
   f i j = sum [(a ! (i,k)) * (b ! (k,j)) | k <- [0..ca-1]]
   in if ca /= rb
      then error "mismatching dimensions!"
      else Matrix [[f i j | j <- [0..cb-1]] | i <- [0..ra-1]]

rowScale :: (Num a) => Matrix a -> Int -> a -> Matrix a
rowScale m i sf = setRow i m $ map (sf *) $ getRow i m

rowSwap :: Matrix a -> (Int, Int) -> Matrix a
rowSwap m (i,j) = let
   row_i = getRow i m
   row_j = getRow j m
   m' = setRow i m row_j
   in setRow j m' row_i

rowAdd :: (Num a) => Matrix a -> Int -> Int -> Matrix a
rowAdd = rowAddBy 1

rowSub :: (Num a) => Matrix a -> Int -> Int -> Matrix a
rowSub = rowAddBy (-1)

rowAddBy :: (Num a) => a -> Matrix a -> Int -> Int -> Matrix a
rowAddBy sf m src tgt = let
   srcRow = getRow src m
   tgtRow = getRow tgt m
   in setRow tgt m $ zipWith (\ x y -> sf * x + y) srcRow tgtRow

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
      m' = rowAddBy (-sf) m 0 i  -- row i reduced by row 0 to have leading 0
      in zeroLead (i-1) m'       -- repeat on other rows
   zeroCoeffs :: Num a => Int -> Matrix a -> Matrix a
   zeroCoeffs 0 m = m
   zeroCoeffs i m = let
      sf = m ! (0,i)
      m' = rowAddBy (-sf) m i 0
      in zeroCoeffs (i-1) m'

-- Zero everywhere
zero :: Num a => (Int, Int) -> Matrix a
zero (r,c) = Matrix $ replicate r $ replicate c 0

-- Identity matrix: 1s along the main diagonal and 0 elsewhere
ident :: Num a => Int -> Matrix a
ident n = let
   row i = replicate i 0 ++ [1] ++ replicate (n-1-i) 0
   in Matrix $ map row [0..n-1]

-- Not sure if this has a name; it's a flipped identity matrix
coIdent :: Num a => Int -> Matrix a
coIdent n = let
   row i = replicate (n-1-i) 0 ++ [1] ++ replicate i 0
   in Matrix $ map row [0..n-1]
