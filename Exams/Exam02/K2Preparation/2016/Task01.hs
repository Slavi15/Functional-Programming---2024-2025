{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List (transpose)

containsAllElements :: [Integer] -> [Integer] -> Bool
containsAllElements xs ys = foldr (\x acc -> acc && x `elem` ys) True xs

allElements :: [Integer] -> [[Integer]] -> Bool
allElements xs = foldr (\ys acc -> acc || (containsAllElements xs ys)) False

findColumns :: [[Integer]] -> Integer
findColumns xss = findColumns' (transpose xss) 0
    where
        findColumns' :: [[Integer]] -> Integer -> Integer
        findColumns' [] count = count
        findColumns' (xs' : xss') count
            | allElements xs' xss = findColumns' xss' (count + 1)
            | otherwise = findColumns' xss' count

-- >>> findColumns [[1,4,3],[4,5,6],[7,4,9]] -- => 1
-- 1
