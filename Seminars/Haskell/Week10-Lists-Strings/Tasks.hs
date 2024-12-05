{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

import Data.Char (chr, ord, isLower, toLower, toUpper, isUpper, isSpace, isAsciiUpper)
import Data.List

whisper :: String -> String
whisper str = [toLower x | x <- str]

removeSpaces :: String -> String
removeSpaces str = [x | x <- str, not $ isSpace x]

switchCase :: Char -> Char
switchCase ch = if isLower ch then toUpper ch else toLower ch

switchCaps :: String -> String
switchCaps str = [switchCase x | x <- str]

-- Task 04

-- >>> encrypt 1 "ABC"   -- "BCD"
-- >>> encrypt 2 "AXYZ"  -- "CZAB"
-- >>> decrypt 1 "BCD"   -- "ABC"
-- >>> decrypt 1 "CZAB"  -- "AXYZ"
-- "BCD"
-- "CZAB"
-- "ABC"
-- "BYZA"

encryptConverter :: Int -> Char -> Char
encryptConverter n ch
    | isAsciiUpper ch = chr (((ord ch - ord 'A' + n) `mod` 26) + ord 'A')
    | otherwise = chr (((ord ch - ord 'a' + n) `mod` 26) + ord 'a')

encrypt :: Int -> String -> String
encrypt n str = [encryptConverter n x | x <- str]

decryptConverter :: Int -> Char -> Char
decryptConverter n ch
    | isAsciiUpper ch = chr (((ord ch - ord 'A' - n) `mod` 26) + ord 'A')
    | otherwise = chr (((ord ch - ord 'a' - n) `mod` 26) + ord 'a')

decrypt :: Int -> String -> String
decrypt n str = [decryptConverter n x | x <- str]

-- Task 05

-- >>> joinWords ' ' ["The", "Sound", "of", "Silence"] -- "The Sound of Silence"
-- >>> joinWords ',' ["One", "Two", "Three", "Four"]   -- "One,Two,Three,Four"
-- "The Sound of Silence"
-- "One,Two,Three,Four"

joinWords :: Char -> [String] -> String
joinWords _ [] = []
joinWords c strs = init $ concat [x ++ [c] | x <- strs]

-- Task 06

-- >>> bwt "BANANA"
-- "NNBAAA"

rotate :: Int -> [a] -> [a]
rotate i str = b ++ a
    where
        (a, b) = splitAt i str

bwt :: String -> String
bwt str = map last (sort (rotations str))
    where
        rotations str' = [rotate i str' | i <- [0 .. length str' - 1]]

-- Task 07

--- >>> indices 1 [1, 2, 3, 1, 4] -- [0, 3]
--- >>> indices 1 []              -- []
-- [0,3]
-- []

indices :: (Eq a) => a -> [a] -> [Int]
indices x xs = [i | (i, y) <- zip [0 ..] xs, x == y]

-- for indexing an array zip [0 ..] xs !!!

-- Task 08

--- >>> lastIndex 1 [1, 2, 7, 1, 5, 4]  -- 3
--- >>> lastIndex 7 [3, 1, 7, 5]        -- 2
--- >>> lastIndex 1 []                  -- error "not in list"
--- >>> lastIndex 3 [2, 4, 7]           -- error "not in list"
-- 3
-- 2
-- Prelude.last: empty list
-- Prelude.last: empty list

lastIndex :: (Eq a) => a -> [a] -> Int
lastIndex x xs = last $ indices x xs

-- Task 09

--- >>> countMin [1, 2, 1, 1, 5, 3] -- 3
--- >>> countMin [3, 4, 2]          -- 1
--- >>> countMin []                 -- 0
-- 3
-- 1
-- 0

countMin :: (Ord a) => [a] -> Int
countMin xs = length [x | x <- xs, x == m]
    where
        m = minimum xs

-- Task 10

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

-- >>> isPrime 7
-- >>> isPrime 8
-- True
-- False

isPrime :: Integer -> Bool
isPrime k
    | k > 1 = null [x | x <- [2 .. isqrt k], k `mod` x == 0]
    | otherwise = False

-- >>> primeReorder [2,3,4,5,6]  -- [2,3,5,4,6]
-- >>> primeReorder "abcd"       -- "abdc"
-- [2,3,5,4,6]
-- "abdc"

primeReorder :: [a] -> [a]
primeReorder xs = [y | (i, y) <- zip [2 ..] xs, isPrime i] ++ [y | (i, y) <- zip [2 ..] xs, not $ isPrime i]

-- Task 11

-- >>> dedup [1, 2, 1]          -- [1, 2]
-- >>> dedup [1, 3, 7, 3, 5, 1] -- [1, 3, 7, 5]
-- [1,2]
-- [1,3,7,5]

dedup :: (Eq a) => [a] -> [a]
dedup xs = dedup' xs []
    where
        dedup' :: (Eq a) => [a] -> [a] -> [a]
        dedup' [] ys = ys
        dedup' (x' : xs') ys
            | x' `notElem` ys = dedup' xs' (ys ++ [x'])
            | otherwise = dedup' xs' ys

-- Task 12

-- >>> merge [1, 3, 7] [2, 4, 6] -- [1, 2, 3, 4, 6, 7]
-- [1,2,3,4,6,7]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

-- >>> mergeSort [2, 1, 3, 7, -16, 5] -- [-16, 1, 2, 3, 5, 7]
-- [-16,1,2,3,5,7]

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs = merge lhs rhs
    where
        mid = length xs `div` 2
        lhs = mergeSort (take mid xs)
        rhs = mergeSort (drop mid xs)

-- Task 13

-- >>> subsets [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = map (x :) subs ++ subs
    where
        subs = subsets xs

-- Task 14

-- >>> pick 2 [1, 2, 3] -- [[1, 2], [1, 3], [2, 3]]
-- [[1,2],[1,3],[2,3]]

pick :: Integer -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x : xs) = map (x :) (pick (k - 1) xs) ++ pick k xs

-- Bonus: generate all permutations i.e [1,2] and [2,1] etc.

-- Task 15

-- >>> maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5                  -- 1.5
-- >>> maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2) -- 16
-- 1.5
-- 16.0

maximize :: (Ord a) => [a -> a] -> a -> a
maximize fs x = maximum [f x | f <- fs]

-- Task 16

-- >>> compose [(+1), (2*)] 7        -- (2 * 7) + 1 = 15
-- >>> compose [(+1), (+1), (+1)] 7  -- 10
-- 15
-- 10

compose :: [a -> a] -> a -> a
compose fs x = foldr (\f x' -> f x') x fs

-- Task 17

-- >>> take 10 $ facts -- [1,2,6,24,120,720,5040,40320,362880,3628800]
-- [1,2,6,24,120,720,5040,40320,362880,3628800]

facts :: [Integer]
facts = facts' 1 1
    where
        facts' :: Integer -> Integer -> [Integer]
        facts' n prev = (n * prev) : facts' (n + 1) (n * prev)

-- Task 18

-- >>> take 10 $ points -- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]

points :: [(Integer, Integer)]
points = [(x, y - x) | y <- [0 ..], x <- [0 .. y]]
