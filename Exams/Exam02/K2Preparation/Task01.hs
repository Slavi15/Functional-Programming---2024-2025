{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List (maximumBy)

getPermutation :: Int -> (Int -> Int) -> [Int]
getPermutation n f = [f x | x <- [0 .. (n - 1)]]

isNPerm :: Int -> (Int -> Int) -> Bool
isNPerm n f = (length $ unique $ (getPermutation n f)) == n

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

getCycle :: Int -> (Int -> Int) -> [Int]
getCycle x f = getCycle' x []
  where
    getCycle' :: Int -> [Int] -> [Int]
    getCycle' current visited
      | current `elem` visited = visited
      | otherwise = getCycle' (f current) (visited ++ [current])

generateCycles :: Int -> (Int -> Int) -> [[Int]]
generateCycles n f = generateCycles' [] [0 .. (n - 1)]
    where
        generateCycles' :: [Int] -> [Int] -> [[Int]]
        generateCycles' _visited [] = []
        generateCycles' visited (x : xs)
            | x `elem` visited = generateCycles' visited xs
            | otherwise = 
                let loop = getCycle x f
                in loop : generateCycles' (visited ++ loop) xs

maxCycle :: Int -> (Int -> Int) -> [Int]
maxCycle n f = maximumBy (compare `on` length) (generateCycles n f)

-- >>> isNPerm 3 (\x -> (3 - x) `mod` 3) -- => True
-- >>> isNPerm 10 (`div` 2) -- => False
-- >>> isNPerm 10 (\x -> (x + 2) `mod` 10) -- => True
-- True
-- False
-- True

-- >>> maxCycle 3 (\x -> (3 - x) `mod` 3) -- => [1, 2]
-- >>> maxCycle 10 (\x -> (x + 2) `mod` 10) -- => [0, 2, 4, 6, 8]
-- >>> maxCycle 10 (\x -> (x + 3) `mod` 10) -- => [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
-- [1,2]
-- [1,3,5,7,9]
-- [0,3,6,9,2,5,8,1,4,7]
