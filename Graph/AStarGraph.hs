
import Data.List (sortOn)
-- data Graph a b = Graph {
   -- vertices :: [Vertex a],
   -- dist     :: (Vertex a, Vertex a) -> b
-- }

-- 'a' is the type of the vertex label
-- 'b' is the type we use to measure distance
data Vertex a b = Vertex {
   label      :: a,
   neighbours :: [(a, b)]
}

type Graph a b = [Vertex a b]

type Heuristic a b = a -> a -> b

findPath :: (Eq a, Ord b, Num b) => Graph a b -> Heuristic a b -> a -> a -> [a]
findPath g h s t = let
   v = getVertex g s
   paths = map (\(u, d) -> (u, d + h s u)) $ neighbours v
   cheap = fst $ head $ sortOn snd paths
   in if s == t
      then [s]
      else s : findPath g h cheap t

getVertex :: Eq a => Graph a b -> a -> Vertex a b
getVertex [] v = error "No vertex with that label!"
getVertex (x:g) v = if label x == v
                    then x
                    else getVertex g v

pathLength :: (Eq a, Num b) => Graph a b -> [a] -> b
pathLength g (start:path) = aux (getVertex g start) path where
   aux _ [] = 0
   aux u (n:vs) = case head $ filter ((n ==) . fst) $ neighbours u of
      (v, d) -> d + aux (getVertex g v) vs

chess = let
   barrier = [(2,4),(2,5),(2,6),(3,6),(4,6),(5,6),(5,5),(5,4),(5,3),(5,2),(4,2),(3,2)]
   squares = [(i,j) | i <- [0..7], j <- [0..7]]
   f (a,b) = 0 <= a && a < 8 && 0 <= b && b < 8
   movement (x,y) = filter f [(x+i, y+j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]
   cost     (x,y) = if elem (x,y) barrier then 100 else 1
   graph   = map (\v -> Vertex v (map (\u -> (u, cost u)) $ movement v)) squares
   disth1 (a,b) (c,d) = abs (a-c) + abs (b-d)
   disth2 (a,b) (c,d) = sqrt ((a-c)^2 + (b-d)^2)
   in findPath graph disth2 (0,0) (7,7)