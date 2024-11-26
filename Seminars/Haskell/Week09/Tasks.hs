{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors

import Prelude hiding (length, foldr, foldl, reverse, init, product, zip, zipWith)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ init [] = init
foldr op init (x : xs) = op x $ foldr op init xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ init [] = init
foldl op init (x : xs) = foldl op (op init x) xs

length :: [a] -> Integer
length xs = foldr (\_ acc -> acc + 1) 0 xs

-- length xs = length' xs 0
--     where
--         length' :: [a] -> Integer -> Integer
--         length' [] acc = acc
--         length' (_ : xs') acc = length' xs' (acc + 1)

exists :: (a -> Bool) -> [a] -> Bool
exists f xs = foldr (||) False (map f xs)

-- exists _ [] = False
-- exists f (x : xs)
--     | f $ x = True
--     | otherwise = exists f xs

forAll :: (a -> Bool) -> [a] -> Bool
forAll f xs = foldr (&&) True (map f xs)

-- forAll _ [] = True
-- forAll f (x : xs)
--     | not (f $ x) = False
--     | otherwise = forAll f xs

member :: (Eq a) => a -> [a] -> Bool
member x xs = exists (== x) xs

-- member el ys = (length $ (filter (== el) ys)) == 1

-- member _ [] = False
-- member x (y : ys)
--     | x == y = True
--     | otherwise = member x ys

pushBack :: a -> [a] -> [a]
pushBack x = foldr (:) [x]

-- pushBack x [] = [x]
-- pushBack x (y : ys) = y : pushBack x ys

reverse :: [a] -> [a]
reverse xs = foldl (flip (:)) [] xs

init' :: [a] -> [a]
init' [] = []
init' (_ : []) = []
init' (x : xs) = x : init' xs

insert :: a -> Int -> [a] -> [a]
insert x 0 ys = x : ys
insert x _ [] = x : []
insert x n (y : ys) = y : insert x (n - 1) ys

product :: (Integral a) => [a] -> a
product = foldl (*) 1

zip :: [a] -> [b] -> [(a, b)]
zip _ [] = []
zip [] _ = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x : xs) (y : ys) = (f x y) : zipWith f xs ys

interleave :: [a] -> [a] -> [a]
interleave _ [] = []
interleave [] _ = []
interleave (x : xs) (y : ys) = x : y : interleave xs ys

nats :: [Integer]
nats = 1 : map (+1) nats
-- nats = [0 ..]

pythagoreanTriples :: (Integral a) => [(a, a, a)]
pythagoreanTriples = [(x, y, z) | z <- [1 ..], y <- [1 .. z], x <- [1 .. y], x^2 + y^2 == z^2]

fibs :: (Integral a) => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- fibs = fibs' 0 1
--     where
--         fibs' :: (Integral a) => a -> a -> [a]
--         fibs' x y = x : fibs' y (x + y)

(!=) :: (Eq a) => a -> a -> Bool
x != y = x /= y

sieve :: (Integral a) => [a] -> [a]
sieve [] = []
sieve (x : xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primes :: (Integral a) => [a]
primes = sieve [2 ..]