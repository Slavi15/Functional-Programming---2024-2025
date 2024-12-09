{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

data Tree = 
    Nil
    | MkTree Tree (Int -> Int) Tree

mapTree :: Tree -> Int -> [Int]
mapTree Nil _ = []
mapTree (MkTree Nil f Nil) acc = [f acc]
mapTree (MkTree lhs f rhs) acc = mapTree lhs (f acc) ++ mapTree rhs (f acc)

t :: Tree
t = MkTree (MkTree (MkTree Nil (*2) Nil) (^2) (MkTree Nil (\x -> x - 3) Nil)) (+1) (MkTree Nil (3^) Nil)

-- >>> mapTree t 2
-- [18,6,27]

-- >>> mapTree t 1
-- [8,1,9]
