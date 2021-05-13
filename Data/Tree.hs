
module AbLib.Data.Tree where

data Tree a = Tree a [Tree a] deriving (Show)

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . showLevel [] where
   showLevel :: Show a => [Bool] -> Tree a -> String
   showLevel trunk (Tree item branches) = let
      thisLine = getTrunkPattern trunk ++ show item ++ "\n"
      nextLines = showBranches trunk branches
      in thisLine ++ nextLines
   
   showBranches :: Show a => [Bool] -> [Tree a] -> String
   showBranches trunk [] = ""
   showBranches trunk [t] = showLevel (trunk ++ [True]) t
   showBranches trunk (t:ts) = showLevel (trunk ++ [False]) t ++ showBranches trunk ts
   
   getTrunkPattern :: [Bool] -> String
   getTrunkPattern [] = ""
   getTrunkPattern [b] = if b then "└─ " else "├─ "
   getTrunkPattern (b:bs) = (if b then "   " else "│  ") ++ getTrunkPattern bs


t = Tree 120 [Tree 12 [Tree 4 [Tree 2 [], Tree 2 []], Tree 3 []], Tree 10 [Tree 5 [], Tree 2 []]]