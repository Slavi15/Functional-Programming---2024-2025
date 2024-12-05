{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

-- >>> allPrefixes [1,2,3]
-- [[],[1],[1,2],[1,2,3]]

allPrefixes :: [a] -> [[a]]
allPrefixes = foldr (\x acc -> [] : map (x :) acc) [[]]

-- >>> allSuffixes [1,2,3]
-- [[1,2,3],[2,3],[3],[]]

allSuffixes :: [a] -> [[a]]
allSuffixes = foldr (\x acc -> (x : head acc) : acc) [[]]

-- >>> lagrange [(1.0, 1.0), (2.0, 4.0), (3.0, 9.0)] 5.0
-- 25.0

lagrange :: [(Float, Float)] -> Float -> Float
lagrange xs x = foldl (\acc (xj, y) -> acc + (y * l xj)) 0 xs
    where
        l xj = foldl step 1 (filter (\(xk, _) -> xk /= xj) xs)
            where
                step acc (xk, _) = acc * ((x - xk) / (xj - xk))
