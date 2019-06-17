{-# LANGUAGE LambdaCase #-}

module AbLib.Math.Simplex where

import Data.Ratio
import GHC.Real (Ratio ((:%)))
import Data.List
import Text.Read (readMaybe)
import Data.Ord
import AbLib.Math.Matrix

showQ :: Rational -> String
showQ (a :% 1) = show a
showQ (a :% b) = show a ++ "/" ++ show b
   
showQ' :: Rational -> String
showQ' q = sign ++ showQ q
   where sign = if q >= 0 then "+" else ""

{- 
 - PROBLEM FORMULATION
 - 
 - Minimise (c^T) * x
 - subject to Ax = b; x >= 0
 - where
 -    x : R^n is the variable vector
 -    A : R^mn is the constaint matrix
 -    b : R^m is the bound vector
 -    c : R^n is the objective vector
 -}

type Var = String
data Atom = Rational :* Var | Q Rational deriving Eq
type Expr = [Atom]
data Objective = Obj OType Expr
data Constraint = Con Expr Ordering Rational deriving Eq
data Problem = LP [Var] Objective [Constraint]

leq :: Expr -> Rational -> Constraint
leq e b = Con e LT b

geq :: Expr -> Rational -> Constraint
geq e b = Con e GT b

equ :: Expr -> Rational -> Constraint
equ e b = Con e EQ b

instance Num Atom where
   Q x + Q y = Q (x + y)
   (x :* v1) + (y :* v2) = if v1 == v2
      then (x + y) :* v1
      else errorWithoutStackTrace "mismatching variables"
   
   negate (Q x) = Q (negate x)
   negate (x :* v) = negate x :* v
   
   Q x * Q y = Q (x * y)
   abs (Q x) = Q (abs x)
   signum (Q x) = Q (signum x)
   fromInteger n = Q (n % 1)

getScale :: Var -> [Atom] -> Rational
getScale v [] = 0
getScale v (s:*x : xs) | x == v = s
getScale v (_:xs) = getScale v xs

data OType = MIN | MAX deriving (Eq)

instance Enum OType where
   fromEnum = \case {MIN -> -1; MAX -> 1}
   toEnum n = if n > 0 then MAX else MIN

instance Show Atom where
   show = \case
      Q r -> showQ r
      1 :* v -> v
      r :* v -> showQ r ++ if '+' `elem` v || '-' `elem` v
         then "(" ++ v ++ ")"
         else v

showExpr :: Expr -> String
showExpr [] = ""
showExpr [x] = show x
showExpr (x:xs) = show x ++ " + " ++ showExpr (xs)

showOrder = \case {LT -> "<="; EQ -> "=="; GT -> ">"}

reduceExpr :: Expr -> Expr
reduceExpr xs = a' : b'
   where
   (a,b) = partition isConst xs
   a' = foldr (+) (Q 0) a
   bs = groupBy (\ x y -> var x == var y) $ sortOn var $ b
   b' = map (foldr1 (+)) bs
   
   isConst :: Atom -> Bool
   isConst = \case { Q _ -> True; _ -> False }
   
   var :: Atom -> String
   var (_ :* v) = v

instance Show Objective where
   show (Obj t e) = show t ++ " " ++ showExpr e
instance Show OType where
   show = \case
      MIN -> "MINIMISE"
      MAX -> "MAXIMISE"

instance Show Constraint where
   show (Con e t b) = showExpr e ++ " " ++ showOrder t ++ " " ++ showQ b

printProblem :: Problem -> IO ()
printProblem (LP x obj cons) = do
   putStrLn $ show obj
   let c:cs = cons
   putStr "subject to: "
   print c
   flip mapM_ cs $ (putStr "            " >>) . print

sample :: Problem
sample = LP
   ["x","y","z"]
   (Obj MAX [2 :* "x", 3 :* "y", 4 :* "z"])
   [ [3:*"x", 2:*"y", 1:*"z"] `leq` 10
   , [2:*"x", 5:*"y", 3:*"z"] `leq` 15
   , [1 :* "x"] `geq` 0
   , [1 :* "y"] `geq` 0
   , [1 :* "z"] `geq` 0 ]

changeVarBy :: Problem -> Var -> Rational -> Problem
changeVarBy (LP x obj cs) v r = LP (map inVar x) (inObj obj) (map inCon cs)
   where
   v' = v ++ showQ' r
   
   inVar :: Var -> Var
   inVar s
      | s == v = v'
      | otherwise = s
   
   inObj :: Objective -> Objective
   inObj (Obj t e) = Obj t $ inExpr e
   
   inExpr :: Expr -> Expr
   inExpr (c:*x : xs) | x == v
      = (c :* v') : Q (c * negate r) : inExpr xs
   inExpr (x:xs) = x : inExpr xs
   inExpr [] = []
   
   inCon :: Constraint -> Constraint
   inCon (Con e t b) = Con (inExpr e) t b

standardize :: Problem -> Problem
standardize p@(LP _ _ cons) = maxObj $ std cons p
   where
   std :: [Constraint] -> Problem -> Problem
   std (Con [r:*x] GT b : cs) p
      | b /= 0 = std cs $ changeVarBy p x $ negate (b/r)
   std (Con e@(_:_:_) t b : cs) p
      | t /= EQ = let
         s  = newSlack p
         r  = toRational $ negate $ fromEnum t
         p' = addVar s
            $ addCon ([1 :* s] `geq` 0)
            $ swpCon (Con e t b) ((r:*s : e) `equ` b) p
         in std cs p'
   std (_:cs) p = std cs p
   std [] p = p
   
   maxObj :: Problem -> Problem
   maxObj (LP vs obj cons) = case obj of
      Obj MIN e -> LP vs (Obj MAX $ map negate e) cons
      _ -> LP vs obj cons

reduce :: Problem -> Problem
reduce (LP vs (Obj t e) cons) = LP vs (Obj t $ delConst e) (map reduceCon cons)
   where
   delConst :: Expr -> Expr
   delConst e = case reduceExpr e of
      (Q _ : e') -> e'
      e' -> e'
   
   reduceCon :: Constraint -> Constraint
   reduceCon (Con e t b) = case reduceExpr e of
      (Q x : e') -> Con e' t (b - x)
      e' -> Con e' t b

delCon :: Constraint -> Problem -> Problem
delCon c (LP vs obj cons) = LP vs obj (delete c cons)

addCon :: Constraint -> Problem -> Problem
addCon c (LP vs obj cons) = LP vs obj (cons ++ [c])

swpCon :: Constraint -> Constraint -> Problem -> Problem
swpCon old new (LP vs obj cons) = LP vs obj $ swpCon' cons
   where
   swpCon' :: [Constraint] -> [Constraint]
   swpCon' [] = []
   swpCon' (c:cs)
      | c == old = new : cs
      | otherwise = c : swpCon' cs

addVar :: Var -> Problem -> Problem
addVar v (LP vs obj cons) = LP (vs++[v]) obj cons

newSlack :: Problem -> Var
newSlack (LP vs _ _) = ('s':) $ show
   $ maximum $ (0:)
   $ map (maybe 0 (+1) . readMaybe . tail)
   $ filter (('s'==) . head) vs

{- STRUCTURE:
 -                  sum  x1  x2 ...  s1  s2 ...
 - Objective Row      0   ?   ? ...   0   0 ...
 - Constraint Row    b1   ?   ? ...   1   0 ...
 - Constraint Row    b2   ?   ? ...   0   1 ...
 - ...              ... ... ... ... ... ... ... ...
 -}
type Tableau = Matrix Rational
tableau :: Problem -> Tableau
tableau p = fromList (objRow obj : map conRow cons')
   where
   LP vs obj cons = reduce $ standardize p
   cons' = flip filter cons $ \case
      Con [_] GT _ -> False
      _ -> True
   
   objRow :: Objective -> [Rational]
   objRow (Obj _ e) = [0] ++ (map negate $ meat e)
   
   conRow :: Constraint -> [Rational]
   conRow (Con e EQ b) = [b] ++ meat e
   conRow _ = errorWithoutStackTrace "Malformed constraint"
   
   meat :: Expr -> [Rational]
   meat e = flip map vs $ flip getScale e
   
solveTableau :: Tableau -> Rational
solveTableau m = if isDone m then solution m
   else solveTableau $ pivot (getPivot m) m
   where
   
   isDone :: Tableau -> Bool
   isDone m = all (>= 0) (tail $ getRow 0 m)
   
   solution :: Tableau -> Rational
   solution m = m ! (0,0)
   
   (//) :: (Fractional a, Eq a) => a -> a -> a
   x // y = if y == 0 then -1 else x / y
   
   getPivot :: Tableau -> (Int,Int)
   getPivot m = let
      f = fst . minimumBy (comparing snd)
      {- Pivot column has most negative value in objective row -}
      c = f $ tail $ zip [0..] $ getRow 0 m
      pvtCol = getCol c m
      sumCol = getCol 0 m
      ratios = zipWith (//) sumCol pvtCol
      {- Pivot row has least non-negative sum -}
      r = f $ filter ((>0) . snd) $ tail $ zip [0..] ratios
      in (r,c)

simplex :: Problem -> Rational
simplex p@(LP _ (Obj t _) _) = (if t == MIN then negate else id) $ solveTableau $ tableau p