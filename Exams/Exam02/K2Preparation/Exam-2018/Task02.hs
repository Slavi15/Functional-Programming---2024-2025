data Tree a = 
    Nil | 
    MkTree a (Tree a) (Tree a)
    deriving (Eq, Show)

foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree _ acc Nil = acc
foldTree f acc (MkTree x lhs rhs) = f (foldTree f acc lhs) x (foldTree f acc rhs)

countCodes :: (Eq t, Num a, Num t) => Tree t -> a
countCodes tr = countCodes' tr 1
    where
        countCodes' Nil _ = 0
        countCodes' (MkTree x lhs rhs) value
            | x == value = 1 + countCodes' lhs value + countCodes' rhs (value + 1)
            | otherwise = countCodes' lhs value + countCodes' rhs (value + 1)

tree :: Tree Int
tree = MkTree 1
        (MkTree 2
            Nil
            (MkTree 3 Nil Nil))
        (MkTree 4
            (MkTree 6 Nil Nil)
            Nil)

-- >>> countCodes tree -- => 3
-- 1
