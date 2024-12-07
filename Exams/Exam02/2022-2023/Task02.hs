{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List (transpose, nub, minimumBy, sort, group)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

-- filt which filters through a list and removes elements that don't occur at least k times.

filt :: Int -> [[Int]] -> [[Int]]
filt k = map head . filter ((>= k) . length) . group . sort

leastColors :: [[Int]] -> (Int -> Int) -> (Int -> Int) -> Int
leastColors matrix f g =
    let
        columns = transpose matrix
        colourLists = map sort (concatMap chooseColors columns) -- sorts every sublist
        removeUniques = filt 2 colourLists
    in
        length $ minimumBy (compare `on` length) removeUniques
    where
        chooseColors :: [Int] -> [[Int]]
        chooseColors col =
            let fColours = nub (map f col)
                gColours = nub (map g col)
            in
                [fColours] ++ [gColours]

-- >>> leastColors [[1, 5], [3, 11]] (`mod` 3) (`mod` 5) -- → 2
-- 2
