{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.List (maximumBy, sortBy)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

addToList :: [(Integer, [(Integer, Integer)])] -> Integer -> (Integer, Integer) -> [(Integer, [(Integer, Integer)])]
addToList [] key (r, h) = [(key, [(r, h)])]
addToList ((k, v) : xs) key (r, h)
    | k == key = (k, v ++ [(r, h)]) : xs
    | otherwise = (k, v) : addToList xs key (r, h)

getVolume :: Integer -> Integer -> Integer
getVolume r h = floor pi * r^2 * h

maxSameVolume :: [(Integer, Integer)] -> [(Integer, Integer)]
maxSameVolume xs = snd $ maximumBy (compare `on` length) (maxSameVolume' xs [])
    where
        maxSameVolume' [] mp = mp
        maxSameVolume' ((r, h) : xs') mp = maxSameVolume' xs' (addToList mp (getVolume r h) (r, h))

validPairs :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
validPairs [] _ = []
validPairs ((r, h) : xs) ml
    | getVolume r h > ml = (r, h) : validPairs xs ml
    | otherwise = validPairs xs ml

getDiff :: Integer -> (Integer, Integer) -> Float
getDiff ml (r, h)
    | diff <= 1 = 3.4028235e38
    | otherwise = diff
    where
        diff = cupHeight - waterHeight
        cupHeight = fromIntegral h
        waterHeight = fromIntegral ml / fromIntegral r^2 / pi

compareFillings :: Integer -> (Integer, Integer) -> (Integer, Integer) -> Ordering
compareFillings ml (r1, h1) (r2, h2)
    | getDiff ml (r1, h1) < getDiff ml (r2, h2) = GT
    | getDiff ml (r1, h1) > getDiff ml (r2, h2) = LT
    | otherwise = EQ

bestPair :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
bestPair xs ml = [sorted !! 0] ++ [sorted !! 1]
    where
        sorted = sortBy (compare `on` getDiff ml) validPairGlasses
        validPairGlasses = validPairs xs ml

glasses :: [(Integer, Integer)]
glasses = [(5, 12), (2, 16), (4, 4), (3, 36), (9, 4), (6, 9)]

-- >>> addToList [(81, [(1, 3), (2, 3)])] 81 (3, 3)
-- [(81,[(1,3),(2,3),(3,3)])]

-- >>> maxSameVolume glasses -- => [(3, 36), (9, 4), (6, 9)]
-- [(3,36),(9,4),(6,9)]

-- >>> bestPair glasses 800 -- => ((5, 12), (6, 9))
-- [(5,12),(6,9)]
